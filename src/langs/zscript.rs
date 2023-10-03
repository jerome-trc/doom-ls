//! Frontend and request/notification handling for ZDoom's [ZScript] language.
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
	rowan::{ast::AstNode, TextRange},
	zdoom::{
		self,
		zscript::{ast, Syn, SyntaxNode},
	},
};
use lsp_types::SymbolKind;
use rayon::prelude::*;
use tracing::debug;

use crate::{
	arena::Arena,
	core::{Core, Project, Source, WorkingWorld},
	data::{SymGraphKey, SymGraphVal, SymPtr, UserSymbol},
	frontend::FrontendContext,
	intern::{NsName, PathIx},
	FxHamt,
};

use self::sema::{Datum, FunctionFlags};

#[derive(Debug, Clone)]
pub(crate) struct IncludeTree {
	pub(crate) root: Option<PathIx>,
	pub(crate) version: zdoom::Version,
	/// Keys are IDs of the "included"; values are IDs of "includers".
	pub(crate) includes: FxHamt<PathIx, PathIx>,
}

impl IncludeTree {
	/// All included files as well as the root.
	pub(crate) fn files<'p>(&'p self, project: &'p Project) -> impl Iterator<Item = &Source> + 'p {
		let root_id = self.root.unwrap();

		std::iter::once(root_id)
			.chain(self.includes.keys().copied())
			.map(|id| project.files.get(&id).unwrap())
	}
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

/// See [`decl`] and [`expand`].
pub(crate) fn front1(world: &WorkingWorld, project_ix: usize, project: &Project) {
	project.zscript.files(project).par_bridge().for_each(|src| {
		let cursor = SyntaxNode::new_root(src.green.as_ref().unwrap().clone());

		let ctx = FrontendContext {
			inner: world,
			project_ix,
			src,
		};

		for top in cursor.children().filter_map(ast::TopLevel::cast) {
			match top {
				ast::TopLevel::ClassDef(classdef) => {
					decl::declare_class(&ctx, classdef);
				}
				ast::TopLevel::ConstDef(constdef) => {
					decl::declare_constant(&ctx, None, constdef);
				}
				ast::TopLevel::StructDef(structdef) => {
					decl::declare_struct(&ctx, None, structdef);
				}
				ast::TopLevel::EnumDef(enumdef) => {
					decl::declare_enum(&ctx, None, enumdef);
				}
				ast::TopLevel::MixinClassDef(mixindef) => {
					decl::declare_mixin_class(&ctx, mixindef);
				}
				ast::TopLevel::ClassExtend(_)
				| ast::TopLevel::StructExtend(_)
				| ast::TopLevel::Include(_)
				| ast::TopLevel::Version(_) => continue,
			}
		}
	});

	#[cfg(debug_assertions)]
	debug!(
		"Finished ZScript declaration pass for project: {}",
		project.root.display()
	);

	project.zscript.files(project).par_bridge().for_each(|src| {
		let cursor = SyntaxNode::new_root(src.green.as_ref().unwrap().clone());

		let ctx = FrontendContext {
			inner: world,
			project_ix,
			src,
		};

		for top in cursor.children().filter_map(ast::TopLevel::cast) {
			match top {
				ast::TopLevel::ClassDef(classdef) => {
					let sym_ptr = ctx.get_symbol(src, classdef.syntax().text_range()).unwrap();
					let u_sym = sym_ptr.as_user().unwrap();

					// Track which classes are in this inheritance hierarchy
					// to guard against circles.
					let hierarchy = vec![sym_ptr.clone()];
					let scope =
						expand::class_inheritance(&ctx, sym_ptr.clone(), classdef, hierarchy);
					let mut bump = ctx.arena.borrow();
					let scope_ptr = Arena::alloc(&mut bump, scope);
					u_sym.scope.store(scope_ptr.as_ptr().unwrap());
				}
				ast::TopLevel::ClassExtend(_)
				| ast::TopLevel::StructExtend(_)
				| ast::TopLevel::MixinClassDef(_)
				| ast::TopLevel::StructDef(_)
				| ast::TopLevel::ConstDef(_)
				| ast::TopLevel::EnumDef(_)
				| ast::TopLevel::Include(_)
				| ast::TopLevel::Version(_) => continue,
			}
		}
	});

	#[cfg(debug_assertions)]
	debug!(
		"Finished ZScript inheritance resolution pass for project: {}",
		project.root.display()
	);

	project.zscript.files(project).par_bridge().for_each(|src| {
		let cursor = SyntaxNode::new_root(src.green.as_ref().unwrap().clone());

		let ctx = FrontendContext {
			inner: world,
			project_ix,
			src,
		};

		for top in cursor.children().filter_map(ast::TopLevel::cast) {
			match top {
				ast::TopLevel::ClassExtend(classext) => expand::extend_class(&ctx, classext),
				ast::TopLevel::StructExtend(structext) => {
					expand::extend_struct(&ctx, structext);
				}
				ast::TopLevel::ClassDef(_)
				| ast::TopLevel::MixinClassDef(_)
				| ast::TopLevel::StructDef(_)
				| ast::TopLevel::ConstDef(_)
				| ast::TopLevel::EnumDef(_)
				| ast::TopLevel::Include(_)
				| ast::TopLevel::Version(_) => continue,
			}
		}
	});

	#[cfg(debug_assertions)]
	debug!(
		"Finished ZScript extension pass for project: {}",
		project.root.display()
	);
}

/// See [`define`].
pub(crate) fn front2(world: &WorkingWorld, project_ix: usize, project: &Project) {
	project.zscript.files(project).par_bridge().for_each(|src| {
		let cursor = SyntaxNode::new_root(src.green.as_ref().unwrap().clone());

		let ctx = FrontendContext {
			inner: world,
			project_ix,
			src,
		};

		for top in cursor.children().filter_map(ast::TopLevel::cast) {
			match top {
				ast::TopLevel::ClassDef(classdef) => {
					let sym_ptr = ctx.get_symbol(src, classdef.syntax().text_range()).unwrap();
					define::class::define(&ctx, sym_ptr, classdef);
				}
				ast::TopLevel::ClassExtend(classext) => {
					let ident = classext.name().unwrap().into();
					let ns_name = NsName::Type(ctx.names.intern(&ident));
					let globals = ctx.global_scope(project_ix);
					let sym_ptr_opt = globals.get(&ns_name).cloned();
					drop(globals);

					if let Some(sym_ptr) = sym_ptr_opt {
						define::class::extend(&ctx, sym_ptr, classext);
					}
				}
				ast::TopLevel::ConstDef(_)
				| ast::TopLevel::StructDef(_)
				| ast::TopLevel::EnumDef(_)
				| ast::TopLevel::MixinClassDef(_)
				| ast::TopLevel::StructExtend(_) => {} // TODO
				ast::TopLevel::Include(_) | ast::TopLevel::Version(_) => continue,
			}
		}
	});

	#[cfg(debug_assertions)]
	debug!(
		"Finished ZScript symbol definition pass for project: {}",
		project.root.display()
	);

	// Finally, check constant initializers and function bodies.

	#[cfg(debug_assertions)]
	debug!(
		"Finished ZScript function check pass for project: {}",
		project.root.display()
	);
}

/// Used for [`Core::decl_text`].
#[must_use]
pub(crate) fn decl_text(ctx: &Core, sym_ptr: &SymPtr, u_sym: &UserSymbol, datum: &Datum) -> String {
	use std::fmt::Write;

	let mut ret = String::new();

	// TODO
	match datum {
		Datum::Class => {
			let _ = write!(ret, "class {}", ctx.names.resolve(u_sym.name));
		}
		Datum::Constant => {
			let _ = write!(ret, "const {}", ctx.names.resolve(u_sym.name));
		}
		Datum::_Enum => {
			let _ = write!(ret, "enum {}", ctx.names.resolve(u_sym.name));
		}
		Datum::_Field(_) => {
			let _ = write!(ret, "{}", ctx.names.resolve(u_sym.name));
		}
		Datum::Function(fn_d) => {
			if let Some(sgv) = ctx
				.ready
				.sym_graph
				.get(&SymGraphKey::Holder(sym_ptr.clone()))
			{
				let SymGraphVal::Symbol(holder) = sgv else {
					unreachable!()
				};

				let holder_name = ctx.names.resolve(holder.as_user().unwrap().name);
				let _ = write!(ret, "{holder_name}.");
			}

			let _ = write!(ret, "{}()", ctx.names.resolve(u_sym.name));

			if fn_d.flags.contains(FunctionFlags::CONST) {
				let _ = write!(ret, " const");
			}
		}
		Datum::_MixinClass => {
			let _ = write!(ret, "mixin class {}", ctx.names.resolve(u_sym.name));
		}
		Datum::_Primitive => {
			let _ = write!(ret, "{}", ctx.names.resolve(u_sym.name));
		}
		Datum::_Struct => {
			let _ = write!(ret, "struct {}", ctx.names.resolve(u_sym.name));
		}
	}

	ret
}

#[must_use]
pub(crate) fn lsp_kind(_: &UserSymbol, datum: &Datum) -> SymbolKind {
	match datum {
		Datum::Class => SymbolKind::CLASS,
		Datum::Constant => SymbolKind::CONSTANT, // TODO: check if enum variant.
		Datum::_Enum => SymbolKind::ENUM,
		Datum::_Field(_) => SymbolKind::FIELD,
		Datum::Function(fn_d) => {
			if fn_d.flags.contains(FunctionFlags::STATIC) {
				SymbolKind::FUNCTION
			} else {
				SymbolKind::METHOD
			}
		}
		Datum::_MixinClass => SymbolKind::INTERFACE,
		Datum::_Primitive => SymbolKind::OPERATOR, // Strictly speaking, this is unreachable.
		Datum::_Struct => SymbolKind::STRUCT,
	}
}

/// Used for [`FrontendContext::diag_location`].
#[must_use]
fn symbol_crit_span(node: &SyntaxNode) -> TextRange {
	match node.kind() {
		Syn::ClassDef => {
			let classdef = ast::ClassDef::cast(node.clone()).unwrap();
			let start = classdef.keyword().text_range().start();

			let end = if let Some(qual) = classdef.qualifiers().last() {
				qual.text_range().end()
			} else if let Some(parent) = classdef.parent_class() {
				parent.text_range().end()
			} else {
				classdef.name().unwrap().text_range().end()
			};

			TextRange::new(start, end)
		}
		Syn::FunctionDecl => {
			let fndecl = ast::FunctionDecl::cast(node.clone()).unwrap();

			let start = if let Some(qual) = fndecl.qualifiers().iter().next() {
				qual.text_range().start()
			} else {
				fndecl.return_types().syntax().text_range().start()
			};

			let end = if let Some(kw) = fndecl.const_keyword() {
				kw.text_range().end()
			} else {
				fndecl.param_list().unwrap().syntax().text_range().end()
			};

			TextRange::new(start, end)
		}
		Syn::VarName => {
			let parent = node.parent().unwrap();
			debug_assert_eq!(parent.kind(), Syn::FieldDecl);
			parent.text_range()
		}
		Syn::StateLabel | Syn::FlagDef | Syn::PropertyDef | Syn::EnumVariant => node.text_range(),
		Syn::StructDef => {
			let structdef = ast::StructDef::cast(node.clone()).unwrap();

			let start = structdef.keyword().text_range().start();

			let end = if let Some(qual) = structdef.qualifiers().last() {
				qual.text_range().end()
			} else {
				structdef.name().unwrap().text_range().end()
			};

			TextRange::new(start, end)
		}
		Syn::StaticConstStat => {
			let sconst = ast::StaticConstStat::cast(node.clone()).unwrap();

			TextRange::new(
				sconst.keywords().0.text_range().start(),
				sconst.name().unwrap().text_range().end(),
			)
		}
		Syn::MixinClassDef => {
			let mixindef = ast::MixinClassDef::cast(node.clone()).unwrap();
			let ident = mixindef.name().unwrap();

			TextRange::new(
				mixindef.keywords().0.text_range().start(),
				ident.text_range().start(),
			)
		}
		Syn::ConstDef => {
			let constdef = ast::ConstDef::cast(node.clone()).unwrap();

			TextRange::new(
				constdef.keyword().text_range().start(),
				constdef.syntax().text_range().end(),
			)
		}
		Syn::EnumDef => {
			let enumdef = ast::EnumDef::cast(node.clone()).unwrap();
			let ident = enumdef.name().unwrap();

			let start = enumdef.keyword().text_range().start();

			let end = if let Some(tspec) = enumdef.type_spec() {
				tspec.0.text_range().end()
			} else {
				ident.text_range().end()
			};

			TextRange::new(start, end)
		}
		other => unreachable!("called `symbol_crit_span` on non-symbol node: {other:#?}"),
	}
}
