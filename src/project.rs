use std::{
	hash::{Hash, Hasher},
	path::{Path, PathBuf},
	sync::Arc,
};

use doomfront::{
	rowan::{GreenNode, SyntaxNode, SyntaxToken, TextSize},
	LangExt,
};
use rustc_hash::FxHashMap;
use tracing::error;

use crate::{
	lines::{LineCol, LineIndex},
	zpath::ZPathBuf,
	zscript, FxIndexSet, LangId,
};

/// A conceptual "compilation unit" of sorts.
///
/// Represents one game modification (or standalone game) made to be loaded
/// to the user's source port of choice.
#[salsa::database(CompilerDatabase, zscript::FrontendDatabase)]
#[derive(Default)]
pub(crate) struct Project {
	/// Not necessarily a directory!
	pub(super) root: PathBuf,
	pub(super) storage: salsa::Storage<Project>,
	/// Can be indexed via [`FileId`].
	/// Any given path can be converted back to a [`lsp_types::Url`].
	pub(super) paths: FxIndexSet<PathBuf>,
	pub(super) zpaths: FxIndexSet<ZPathBuf>,
	pub(super) sources: FxIndexSet<FileId>,
}

impl salsa::Database for Project {}

impl Project {
	#[must_use]
	pub(crate) fn new(root: PathBuf) -> Self {
		Self {
			root,
			..Default::default()
		}
	}

	pub(crate) fn build_include_trees(&mut self) {
		let dir_reader = match std::fs::read_dir(self.root()) {
			Ok(d_r) => d_r,
			Err(err) => {
				error!(
					"Failed to build include trees for project: `{}` - {err}",
					self.root().display()
				);

				return;
			}
		};

		for result in dir_reader {
			let d_ent = match result {
				Ok(e) => e,
				Err(err) => {
					error!("Failed to inspect a project file: {err}");
					continue;
				}
			};

			let de_path = d_ent.path();
			let Some(fstem) = de_path.file_stem() else { continue; };

			if fstem.eq_ignore_ascii_case("ZSCRIPT") && !de_path.is_dir() {
				match self.rebuild_zscript_include_tree(de_path) {
					Ok(diags) => {
						let _ = diags; // TODO
					}
					Err(err) => {
						error!("Failed to read ZSCRIPT lump: {err}");
						continue;
					}
				}
			}
		}
	}

	#[must_use]
	pub(crate) fn root(&self) -> &Path {
		&self.root
	}

	#[must_use]
	pub(crate) fn get_path(&self, id: FileId) -> Option<&Path> {
		self.paths.get_index(id.0 as usize).map(|p| p.as_path())
	}

	/// Note that this returns `None` if no source has been registered under
	/// the file ID corresponding to `path` yet.
	#[must_use]
	pub(crate) fn get_fileid(&self, path: &Path) -> Option<FileId> {
		self.paths
			.get_index_of(path)
			.map(|i| FileId(i as u32))
			.filter(|file_id| self.sources.contains(file_id))
	}

	pub(crate) fn intern_path(&mut self, path: &Path) -> FileId {
		if let Some(i) = self.paths.get_index_of(path) {
			return FileId(i as u32);
		}

		self.intern_path_impl(path.to_path_buf())
	}

	pub(crate) fn intern_pathbuf(&mut self, pathbuf: PathBuf) -> FileId {
		if let Some(i) = self.paths.get_index_of(&pathbuf) {
			return FileId(i as u32);
		}

		self.intern_path_impl(pathbuf)
	}

	pub(crate) fn on_file_delete(&mut self, path: PathBuf) {
		if let Some(file_id) = self.get_fileid(&path) {
			self.sources.remove(&file_id);
		}
	}

	#[must_use]
	fn intern_path_impl(&mut self, p: PathBuf) -> FileId {
		let ret = self.paths.insert_full(p.clone()).0;
		let z = self.zpaths.insert_full(ZPathBuf::new(p)).0;
		debug_assert_eq!(ret, z);
		FileId(ret as u32)
	}
}

pub trait Upcast<T: ?Sized> {
	fn upcast(&self) -> &T;
}

impl Upcast<dyn zscript::Frontend> for Project {
	fn upcast(&self) -> &(dyn zscript::Frontend + 'static) {
		self
	}
}

#[salsa::query_group(CompilerDatabase)]
pub(crate) trait Compiler: zscript::Frontend + Upcast<dyn zscript::Frontend> {
	#[salsa::input]
	fn source(&self, key: FileId) -> Source;

	#[salsa::interned]
	fn gfile(&self, file: GreenFile) -> GreenFileKey;

	#[must_use]
	fn get_gfile(&self, key: FileId) -> GreenFileKey;

	#[must_use]
	fn try_get_gfile(&self, key: FileId) -> Option<GreenFileKey>;

	#[must_use]
	fn symbol_table(&self, key: FileId) -> Option<Arc<SymbolTable>>;
}

#[must_use]
fn get_gfile(db: &dyn Compiler, key: FileId) -> GreenFileKey {
	db.try_get_gfile(key).unwrap()
}

#[must_use]
fn try_get_gfile(db: &dyn Compiler, key: FileId) -> Option<GreenFileKey> {
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

	let ret = db.gfile(GreenFile {
		lang: source.lang,
		root,
		lndx: source.lndx,
	});

	Some(ret)
}

#[must_use]
fn symbol_table(db: &dyn Compiler, key: FileId) -> Option<Arc<SymbolTable>> {
	let Some(gf_k) = db.try_get_gfile(key) else { return None; };
	let gfile = db.lookup_gfile(gf_k);

	match gfile.lang {
		LangId::ZScript => Some({
			let up: &dyn zscript::Frontend = db.upcast();
			Arc::new(up.zscript_symbol_table(key, gfile))
		}),
		LangId::Unknown => None,
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FileId(
	/// Corresponds to an index in [`Project::paths`].
	pub(super) u32,
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Source {
	pub(crate) lang: LangId,
	pub(crate) text: Arc<str>,
	pub(crate) lndx: Arc<LineIndex>,
}

impl Hash for Source {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.lang.hash(state);
		self.text.hash(state);
		Arc::as_ptr(&self.lndx).hash(state);
	}
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FilePos {
	pub(crate) file: FileId,
	pub(crate) pos: TextSize,
}

/// A "qualified name", whose content is tagged with a namespace.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum QName {
	Type(Box<str>),
	// TODO: Symbolic constants, CVars, GLDEF objects, et cetera...
}

impl QName {
	#[must_use]
	pub(crate) fn new_for_type(string: &str) -> Self {
		Self::Type(string.to_owned().into_boxed_str())
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SymbolKey {
	ZScript(zscript::SymbolKey),
}

pub(crate) type SymbolTable = FxHashMap<QName, SymbolKey>;
