//! The core of DoomLS' incremental compiler.

use std::{
	hash::{Hash, Hasher},
	path::Path,
	sync::Arc,
};

use crate::{
	intern::{GreenFile, GreenFileKey, Interner, InternerDatabase},
	lines::LineIndex,
	LangId,
};

#[salsa::database(InternerDatabase, CompilerDatabase)]
#[derive(Default)]
pub(crate) struct DatabaseImpl {
	storage: salsa::Storage<DatabaseImpl>,
}

impl salsa::Database for DatabaseImpl {}

#[salsa::query_group(CompilerDatabase)]
pub(crate) trait Compiler: Interner {
	#[salsa::input]
	fn source(&self, key: Box<Path>) -> Source;

	fn try_file(&self, key: Box<Path>) -> Option<GreenFileKey>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Source {
	pub(crate) lang: LangId,
	pub(crate) text: Arc<str>,
	pub(crate) lndx: Arc<LineIndex>,
}

impl Hash for Source {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.lang.hash(state);
		Arc::as_ptr(&self.text).hash(state);
		Arc::as_ptr(&self.lndx).hash(state);
	}
}

#[must_use]
fn try_file(db: &dyn Compiler, key: Box<Path>) -> Option<GreenFileKey> {
	let source = db.source(key);

	let parser = match source.lang {
		LangId::ZScript => doomfront::zdoom::zscript::parse::file,
		LangId::Unknown => return None,
	};

	let lex_ctx = if let LangId::ZScript = source.lang {
		// TODO: Per-folder user config for ZScript version.
		doomfront::zdoom::lex::Context::ZSCRIPT_LATEST
	} else {
		doomfront::zdoom::lex::Context::NON_ZSCRIPT
	};

	let root = match source.lang {
		LangId::ZScript => doomfront::parse::<doomfront::zdoom::zscript::Syn>(
			source.text.as_ref(),
			parser,
			lex_ctx,
		)
		.into_inner(),
		LangId::Unknown => unreachable!(),
	};

	let ret = db.intern_file(GreenFile {
		lang: source.lang,
		root,
		lndx: source.lndx,
	});

	Some(ret)
}
