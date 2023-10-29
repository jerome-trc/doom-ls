//! Frontend and request/notification handling for ZDoom's [ZScript] language.
//!
//! [ZScript]: doomfront::zdoom::zscript

pub(crate) mod decl;
pub(crate) mod define;
pub(crate) mod docsym;
pub(crate) mod expand;
pub(crate) mod goto_def;
pub(crate) mod help;
pub(crate) mod hover;
pub(crate) mod inctree;
pub(crate) mod internal;
pub(crate) mod parse;
pub(crate) mod sema;
pub(crate) mod semtok;

use doomfront::{
	rowan::ast::AstNode,
	zdoom::{
		self,
		zscript::{ast, SyntaxNode},
	},
};
use petgraph::prelude::DiGraphMap;
use rayon::prelude::*;
use tracing::debug;

use crate::{
	arena::Arena,
	core::{Project, Source, WorkingWorld},
	frontend::FrontendContext,
	intern::{NsName, PathIx},
};

#[derive(Debug, Clone)]
pub(crate) struct IncludeTree {
	pub(crate) root: Option<PathIx>,
	pub(crate) version: zdoom::Version,
	/// Edges are from "includer" to "included", and hold the span of the include
	/// directive's AST node.
	/// Note that a file ID may be a present node even if there is no file with
	/// that ID in any project in the workspace; these nodes will still have edges.
	pub(crate) includes: DiGraphMap<PathIx, lsp_types::Range>,
}

impl IncludeTree {
	#[must_use]
	pub(crate) fn invalidate_includer(&mut self, file_id: PathIx) -> Vec<PathIx> {
		debug_assert!(self.includes.contains_node(file_id));

		let prev_nb: Vec<_> = self
			.includes
			.neighbors_directed(file_id, petgraph::Direction::Outgoing)
			.collect();

		for nb in prev_nb.iter().copied() {
			self.includes.remove_edge(file_id, nb);
		}

		prev_nb
	}

	/// All included files as well as the root.
	pub(crate) fn files<'p>(&'p self, project: &'p Project) -> impl Iterator<Item = &Source> + 'p {
		if let Some(r) = self.root {
			debug_assert!(self.includes.contains_node(r));
		} else {
			debug_assert_eq!(self.includes.node_count(), 0);
		}

		self.includes
			.nodes()
			.filter_map(|file_id| project.files.get(&file_id))
	}
}

impl Default for IncludeTree {
	fn default() -> Self {
		Self {
			root: None,
			version: zdoom::Version::V2_4_0,
			includes: DiGraphMap::default(),
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
				ast::TopLevel::EnumDef(enumdef) => {
					let sym_ptr = ctx.get_symbol(src, enumdef.syntax().text_range()).unwrap();
					define::enumeration::define(&ctx, sym_ptr, enumdef);
				}
				ast::TopLevel::StructDef(structdef) => {
					let sym_ptr = ctx
						.get_symbol(src, structdef.syntax().text_range())
						.unwrap();
					define::structure::define(&ctx, sym_ptr, structdef);
				}
				ast::TopLevel::ConstDef(_) => {}
				ast::TopLevel::MixinClassDef(mixindef) => {
					let sym_ptr = ctx.get_symbol(src, mixindef.syntax().text_range()).unwrap();
					let ident = mixindef.name().unwrap();
					ctx.make_ref_to(ident.text_range(), sym_ptr);
				}
				ast::TopLevel::StructExtend(structext) => {} // TODO
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
