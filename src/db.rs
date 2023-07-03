use std::path::Path;

use doomfront::{
	rowan::{GreenNode, SyntaxNode, SyntaxToken, TextSize},
	LangExt,
};

use crate::intern::{Interner, InternerDatabase};

#[salsa::database(InternerDatabase, CompilerDatabase)]
#[derive(Default)]
pub(crate) struct DatabaseImpl {
	storage: salsa::Storage<DatabaseImpl>,
}

impl salsa::Database for DatabaseImpl {}

#[salsa::query_group(CompilerDatabase)]
pub(crate) trait Compiler: Interner {
	#[salsa::input]
	fn file(&self, key: Box<Path>) -> GreenFile;

	#[salsa::input]
	fn try_file(&self, key: Box<Path>) -> Option<GreenFile>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LangId {
	ZScript,
	Unknown,
}

#[derive(Debug, Clone)]
pub(crate) struct GreenFile {
	pub(crate) lang: LangId,
	pub(crate) root: GreenNode,
	/// Byte offset of the beginning of each line
	/// (except the first, which always has offset 0).
	pub(crate) newlines: Box<[TextSize]>,
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
