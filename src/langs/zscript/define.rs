//! The ZScript frontend's third phase; definition.
//!
//! Semantic checks on function bodies, actor defaults, and actor state machines
//! happen here. Initializers for symbolic constants are also checked and evaluated.
//!
//! Additionally, name-to-symbol references are added to the graph.

pub(crate) mod class;
pub(crate) mod enumeration;
pub(crate) mod field;
pub(crate) mod function;
pub(crate) mod structure;

use doomfront::zdoom::zscript::{ast, Syntax};
use lsp_types::DiagnosticSeverity;
use rustc_hash::FxHashMap;

use crate::{data::SymPtr, frontend::FrontendContext};

use super::sema;

#[derive(Debug)]
pub(crate) struct Holder {
	pub(crate) ptr: SymPtr,
	pub(crate) scope: sema::Scope,
}

#[must_use]
fn member_qual_set(
	ctx: &FrontendContext,
	quals: &ast::MemberQuals,
) -> FxHashMap<Syntax, ast::MemberQual> {
	let mut qualset = FxHashMap::default();

	for qual in quals.iter() {
		let displaced = qualset.insert(qual.kind(), qual);

		if let Some(d_q) = displaced {
			ctx.raise(ctx.src.diag_builder(
				d_q.text_range(),
				DiagnosticSeverity::INFORMATION,
				"repeating a qualifier does nothing".to_string(),
			));
		}
	}

	qualset
}
