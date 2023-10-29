//! "Symbol" data structures; pieces of data declared and inspected by frontends.

use doomfront::rowan::SyntaxKind;
use parking_lot::RwLock;

use crate::Scope;

use super::{NsName, SymbolId};

#[derive(Debug)]
pub struct Symbol {
	pub id: SymbolId,
	pub name: NsName,
	pub syn: SyntaxKind,
	pub datum: SymDatum,
	pub scope: Option<RwLock<Scope>>,
	// TODO: test if, in an end-to-end compilation, it's faster to use
	// `Option<APtr<Scope>>` here instead of a lock.
}

#[derive(Debug)]
pub enum SymDatum {
	// TODO
}
