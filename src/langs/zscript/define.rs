//! The ZScript frontend's third phase; definition.
//!
//! Semantic checks on function bodies, actor defaults, and actor state machines
//! happen here. Initializers for symbolic constants are also checked and constant-
//! evaluated.
//!
//! Additionally, name-to-symbol references are added to the graph.

pub(crate) mod function;

use doomfront::zdoom::zscript::ast;
use lsp_types::OneOf;

use crate::{
	core::{DefIx, Definition, SymIx},
	frontend::FrontendContext,
};

use super::sema::Datum;

pub(crate) fn define_class(ctx: &FrontendContext, sym_ix: SymIx, ast: ast::ClassDef) {
	let OneOf::Left(sym) = ctx.symbol(sym_ix) else {
		unreachable!()
	};

	sym.def.store(DefIx::PENDING);

	let name_tok = ast.name().unwrap();
	let ix = ctx.defs.push(Definition::ZScript(Datum::Class));
	let def_ix = DefIx(ix as u32);
	sym.def.store(def_ix);

	ctx.make_ref_to(name_tok.text_range(), sym_ix);

	if let Some(parent_ident) = ast.parent_class() {
		if let Some(parent_ix) = ctx.parent_of(sym_ix) {
			ctx.make_ref_to(parent_ident.text_range(), parent_ix);
		}
	}
}
