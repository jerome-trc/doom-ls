//! The interner part of DoomLS' incremental compiler and the things its stores.

use std::sync::Arc;

use doomfront::{
	rowan::{GreenNode, SyntaxNode, SyntaxToken, TextSize},
	LangExt,
};

use crate::LangId;

#[salsa::query_group(InternerDatabase)]
pub(crate) trait Interner {
	#[salsa::interned]
	fn intern_file(&self, file: GreenFile) -> GreenFileKey;
}

// Values //////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct GreenFile {
	pub(crate) lang: LangId,
	pub(crate) root: GreenNode,
	/// Byte offset of the beginning of each line
	/// (except the first, which always has offset 0).
	pub(crate) newlines: Arc<[TextSize]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct GreenFileKey(salsa::InternId);

impl salsa::InternKey for GreenFileKey {
	fn from_intern_id(v: salsa::InternId) -> Self {
		Self(v)
	}

	fn as_intern_id(&self) -> salsa::InternId {
		self.0
	}
}

impl GreenFile {
	#[must_use]
	pub(crate) fn token_at<L: LangExt>(&self, pos: lsp_types::Position) -> Option<SyntaxToken<L>> {
		let ix_line = pos.line as usize;

		let file_start_offs = if ix_line != 0 {
			match self.newlines.get(ix_line - 1) {
				Some(n) => *n,
				None => return None,
			}
		} else {
			TextSize::from(0)
		};

		let line_start_offs = TextSize::from(pos.character);
		let cursor = SyntaxNode::<L>::new_root(self.root.clone());

		cursor
			.token_at_offset(file_start_offs + line_start_offs)
			.next()
	}
}
