//! The interner part of DoomLS' incremental compiler and the values it stores.

use std::{
	hash::{Hash, Hasher},
	sync::Arc,
};

use doomfront::{
	rowan::{GreenNode, SyntaxNode, SyntaxToken},
	LangExt,
};

use crate::{
	lines::{LineCol, LineIndex},
	LangId,
};

#[salsa::query_group(InternerDatabase)]
pub(crate) trait Interner {
	#[salsa::interned]
	fn intern_file(&self, file: GreenFile) -> GreenFileKey;
}

// Values //////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct GreenFile {
	pub(crate) lang: LangId,
	pub(crate) root: GreenNode,
	pub(crate) lndx: Arc<LineIndex>,
}

impl Hash for GreenFile {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.lang.hash(state);
		self.root.hash(state);
		Arc::as_ptr(&self.lndx).hash(state);
	}
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
		let linecol = LineCol {
			line: pos.line,
			col: pos.character,
		};

		let Some(offs) = self.lndx.offset(linecol) else { return None; };

		SyntaxNode::<L>::new_root(self.root.clone())
			.token_at_offset(offs)
			.next()
	}
}

#[cfg(test)]
mod test {
	use doomfront::zdoom::zscript;
	use lsp_types::Position;

	use super::*;

	#[test]
	fn smoke_token_at() {
		const SOURCE: &str = r##"const SOMETHING = 2.0f * 1.0f;
const SOMETHING = 1.0f * 2.0f;

struct Something {}
"##;

		let gfile = GreenFile {
			lang: LangId::ZScript,
			root: doomfront::parse(
				SOURCE,
				doomfront::zdoom::zscript::parse::file,
				doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
			)
			.into_inner(),
			lndx: Arc::new(LineIndex::new(SOURCE)),
		};

		let t = gfile
			.token_at::<zscript::Syn>(Position {
				line: 3,
				character: 3,
			})
			.unwrap();

		assert_eq!(t.kind(), zscript::Syn::KwStruct);
	}
}
