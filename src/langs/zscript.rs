//! Request and notification handling for ZDoom's [ZScript] language.
//!
//! [ZScript]: doomfront::zdoom::zscript

pub(crate) mod decl;
pub(crate) mod define;
pub(crate) mod docsym;
pub(crate) mod expand;
pub(crate) mod hover;
pub(crate) mod inctree;
pub(crate) mod internal;
pub(crate) mod parse;
pub(crate) mod sema;
pub(crate) mod semtok;

use doomfront::{
	rowan::{ast::AstNode, Language, NodeOrToken},
	zdoom::{
		self,
		zscript::{ast, Syn, SyntaxNode},
	},
};
use lsp_types::OneOf;
use rayon::prelude::*;

use crate::{
	core::{FileSpan, Project, Scope, SymIx, WorkingWorld},
	frontend::FrontendContext,
	intern::PathIx,
	FxHamt,
};

use super::LangId;

#[derive(Debug, Clone)]
pub(crate) struct IncludeTree {
	pub(crate) root: Option<PathIx>,
	pub(crate) version: zdoom::Version,
	/// Keys are IDs of the "included"; values are IDs of "includers".
	pub(crate) includes: FxHamt<PathIx, PathIx>,
}

impl Default for IncludeTree {
	fn default() -> Self {
		Self {
			root: None,
			version: zdoom::Version::V2_4_0,
			includes: FxHamt::default(),
		}
	}
}

/// See [`decl`].
pub(crate) fn front1(world: &WorkingWorld, project_ix: usize, project: &Project) {
	let root_id = project.zscript.root.unwrap();
	let root_src = project.files.get(&root_id).unwrap();
	let cursor = SyntaxNode::new_root(root_src.green.as_ref().unwrap().clone());

	let ctx = FrontendContext {
		inner: world,
		project_ix,
		src: root_src,
	};

	front1_impl(&ctx, cursor);

	project
		.zscript
		.includes
		.keys()
		.par_bridge()
		.for_each(|included| {
			let src = project.files.get(included).unwrap();
			let cursor = SyntaxNode::new_root(src.green.as_ref().unwrap().clone());

			let ctx = FrontendContext {
				inner: world,
				project_ix,
				src,
			};

			front1_impl(&ctx, cursor);
		});
}

fn front1_impl(ctx: &FrontendContext, cursor: SyntaxNode) {
	for top in cursor.children().filter_map(ast::TopLevel::cast) {
		match top {
			ast::TopLevel::ClassDef(classdef) => {
				decl::declare_class(ctx, classdef);
			}
			ast::TopLevel::ConstDef(constdef) => {
				decl::declare_constant(ctx, None, constdef);
			}
			ast::TopLevel::StructDef(structdef) => {
				decl::declare_struct(ctx, None, structdef);
			}
			ast::TopLevel::EnumDef(enumdef) => {
				decl::declare_enum(ctx, None, enumdef);
			}
			ast::TopLevel::MixinClassDef(mixindef) => {
				decl::declare_mixin_class(ctx, mixindef);
			}
			ast::TopLevel::ClassExtend(class_ext) => {
				ctx.extensions.push(FileSpan {
					file_id: ctx.src.id,
					span: class_ext.syntax().text_range(),
				});
			}
			ast::TopLevel::StructExtend(struct_ext) => {
				ctx.extensions.push(FileSpan {
					file_id: ctx.src.id,
					span: struct_ext.syntax().text_range(),
				});
			}
			ast::TopLevel::Include(_) | ast::TopLevel::Version(_) => continue,
		}
	}
}

/// See [`expand`].
pub(crate) fn front2(world: &WorkingWorld, project_ix: usize, cur_project: &Project) {
	world
		.decls
		.iter()
		.enumerate()
		.par_bridge()
		.for_each(|(i, sym)| {
			let sym_ix = SymIx(i as i32);

			if sym.lang != LangId::ZScript || (sym.project as usize) != project_ix {
				return;
			}

			let src = cur_project.files.get(&sym.id.file_id).unwrap();

			let ctx = FrontendContext {
				inner: world,
				project_ix,
				src,
			};

			let file_node = SyntaxNode::new_root(src.green.as_ref().unwrap().clone());

			let sym_elem = file_node.covering_element(sym.id.span);

			let sym_node = match sym_elem {
				NodeOrToken::Node(n) => n,
				NodeOrToken::Token(t) => t.parent().unwrap(),
			};

			debug_assert_eq!(sym_node.kind(), Syn::kind_from_raw(sym.syn));

			let (sender, scope_opt) = match sym_node.kind() {
				Syn::ClassDef => {
					let OneOf::Left(sender) = ctx.get_scope_or_sender(sym.id.0) else {
						return;
					};

					let classdef = ast::ClassDef::cast(sym_node).unwrap();
					let scope_opt = expand::declare_class_scope(&ctx, sym_ix, classdef);
					(sender, scope_opt)
				}
				Syn::StructDef => {
					let OneOf::Left(sender) = ctx.get_scope_or_sender(sym.id.0) else {
						return;
					};

					let structdef = ast::StructDef::cast(sym_node).unwrap();
					let mut scope = Scope::default();
					expand::declare_struct_innards(&ctx, sym_ix, &mut scope, structdef.innards());
					(sender, Some(scope))
				}
				Syn::MixinClassDef => {
					let OneOf::Left(sender) = ctx.get_scope_or_sender(sym.id.0) else {
						return;
					};

					let mixindef = ast::MixinClassDef::cast(sym_node).unwrap();
					let scope_opt = expand::declare_mixin_class_innards(&ctx, sym_ix, mixindef);
					(sender, scope_opt)
				}
				_ => return,
			};

			let Some(scope) = scope_opt else {
				return;
			};

			ctx.scopes.insert(sym.id.0, scope.clone());
			// If any other threads are waiting for this scope to be filled, service them.
			while let Ok(()) = sender.try_send(scope.clone()) {}
		});
}

pub(crate) fn extend_classes_and_structs(world: &WorkingWorld, project_ix: usize) {
	world.extensions.iter().par_bridge().for_each(|ext| {
		let ctx = FrontendContext {
			inner: world,
			project_ix,
			src: world.get_file(project_ix, *ext),
		};

		let node = ctx.src.node_covering::<Syn>(ext.span);

		match node.kind() {
			Syn::ClassExtend => {
				let class_ext = ast::ClassExtend::cast(node).unwrap();
				expand::extend_class(&ctx, class_ext);
			}
			Syn::StructExtend => {
				let struct_ext = ast::StructExtend::cast(node).unwrap();
				expand::extend_struct(&ctx, struct_ext);
			}
			_ => unreachable!(),
		}
	});
}

/// See [`define`].
pub(crate) fn front3(world: &WorkingWorld, project_ix: usize, cur_project: &Project) {
	world
		.decls
		.iter()
		.enumerate()
		.par_bridge()
		.for_each(|(i, sym)| {
			if sym.lang != LangId::ZScript || (sym.project as usize) != project_ix {
				return;
			}

			let src = cur_project.files.get(&sym.id.file_id).unwrap();

			let ctx = FrontendContext {
				inner: world,
				project_ix,
				src,
			};

			let sym_ix = SymIx(i as i32);

			match Syn::kind_from_raw(sym.syn) {
				Syn::ClassDef => {
					let sym_node = src.node_covering(sym.id.span);
					let classdef = ast::ClassDef::cast(sym_node).unwrap();
					define::define_class(&ctx, sym_ix, classdef);
				}
				Syn::FunctionDecl => {
					let sym_node = src.node_covering(sym.id.span);
					let fndecl = ast::FunctionDecl::cast(sym_node).unwrap();
					let _ = define::function::define(&ctx, sym_ix, fndecl);
				}
				Syn::VarName
				| Syn::StateLabel
				| Syn::EnumVariant
				| Syn::PropertyDef
				| Syn::ConstDef
				| Syn::FlagDef
				| Syn::EnumDef
				| Syn::StructDef
				| Syn::StaticConstStat
				| Syn::MixinClassDef => {} // TODO
				other => unreachable!("expected a ZScript symbol, found: {other:#?}"),
			}
		});
}
