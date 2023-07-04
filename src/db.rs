//! The core of DoomLS' incremental compiler.

use std::{path::Path, sync::Arc};

use crate::{
	intern::{GreenFile, GreenFileKey, Interner, InternerDatabase},
	scan::compute_newlines,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Source {
	pub(crate) text: Arc<str>,
	pub(crate) lang: LangId,
}

#[must_use]
fn try_file(db: &dyn Compiler, key: Box<Path>) -> Option<GreenFileKey> {
	let source = db.source(key);

	let parser = match source.lang {
		LangId::ZScript => doomfront::zdoom::zscript::parse::file,
		LangId::Unknown => return None,
	};

	let lex_ctx = if let LangId::ZScript = source.lang {
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
		LangId::Unknown => return None,
	};

	let ret = db.intern_file(GreenFile {
		lang: source.lang,
		root,
		newlines: compute_newlines(source.text.as_ref()).into(),
	});

	Some(ret)
}
