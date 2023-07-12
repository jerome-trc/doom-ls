use std::path::{Path, PathBuf};

use doomfront::{
	rowan::{GreenNode, SyntaxNode, SyntaxToken, TextSize},
	LangExt, ParseError,
};
use lsp_types::Diagnostic;
use rayon::prelude::{ParallelBridge, ParallelIterator};
use rustc_hash::{FxHashMap, FxHashSet};
use tracing::error;

use crate::{
	lines::{LineCol, LineIndex},
	names::{IName, StringInterner},
	zpath::{ZPath, ZPathBuf},
	zscript, FxIndexSet, LangId,
};

#[derive(Debug)]
pub(crate) struct Project {
	root: PathBuf,
	paths: PathInterner,
	files: FxHashMap<FileId, SourceFile>,
	/// A `SourceFile`'s ID gets added whenever a `textDocument/didChange`
	/// notification comes in. It is left until the next time the client makes
	/// a request, at which point that file's symbols are invalidated.
	dirty: FxHashSet<FileId>,
	symbols: Scope,
	// Languages ///////////////////////////////////////////////////////////////
	zscript: zscript::Storage,
}

impl Project {
	#[must_use]
	pub(crate) fn new(root: PathBuf) -> Self {
		Self {
			root,
			paths: PathInterner::default(),
			files: FxHashMap::default(),
			dirty: FxHashSet::default(),
			symbols: Scope::default(),
			zscript: zscript::Storage::default(),
		}
	}

	#[must_use]
	pub(crate) fn root(&self) -> &Path {
		&self.root
	}

	#[must_use]
	pub(crate) fn get_path(&self, id: FileId) -> Option<&Path> {
		self.paths
			.case
			.get_index(id.0 as usize)
			.map(|p| p.as_path())
	}

	/// Note that this returns `None` if no source has been registered under
	/// the file ID corresponding to `path` yet.
	#[must_use]
	pub(crate) fn get_fileid(&self, path: &Path) -> Option<FileId> {
		self.paths
			.case
			.get_index_of(path)
			.map(|i| FileId(i as u32))
			.filter(|file_id| self.files.contains_key(file_id))
	}

	/// Note that this returns `None` if no source has been registered under
	/// the file ID corresponding to `path` yet.
	#[must_use]
	pub(crate) fn _get_fileid_z(&self, path: &ZPath) -> Option<FileId> {
		self.get_pathid_z(path)
			.filter(|file_id| self.files.contains_key(file_id))
	}

	/// A counterpart to [`Self::get_fileid_z`] which does not check for
	/// the presence of a source file.
	#[must_use]
	pub(crate) fn get_pathid_z(&self, path: &ZPath) -> Option<FileId> {
		self.paths
			.nocase
			.get_index_of(path)
			.map(|i| FileId(i as u32))
	}

	pub(crate) fn intern_path(&mut self, path: &Path) -> FileId {
		if let Some(i) = self.paths.case.get_index_of(path) {
			return FileId(i as u32);
		}

		self.paths.intern(path.to_path_buf())
	}

	pub(crate) fn intern_pathbuf(&mut self, pathbuf: PathBuf) -> FileId {
		if let Some(i) = self.paths.case.get_index_of(&pathbuf) {
			return FileId(i as u32);
		}

		self.paths.intern(pathbuf)
	}

	#[must_use]
	pub(crate) fn get_file(&self, file_id: FileId) -> Option<&SourceFile> {
		self.files.get(&file_id)
	}

	#[must_use]
	pub(crate) fn get_file_mut(&mut self, file_id: FileId) -> Option<&mut SourceFile> {
		self.files.get_mut(&file_id)
	}

	pub(crate) fn set_file(&mut self, file_id: FileId, sfile: SourceFile) -> Option<SourceFile> {
		self.files.insert(file_id, sfile)
	}

	#[must_use]
	pub(crate) fn lookup_global(&self, qname: &IName) -> Option<&Datum> {
		self.symbols.get(qname)
	}

	pub(crate) fn all_files(&self) -> impl Iterator<Item = (FileId, &SourceFile)> {
		self.files.iter().map(|t| (*t.0, t.1))
	}

	#[must_use = "iterators are lazy and do nothing unless consumed"]
	pub(crate) fn all_files_par(&self) -> impl ParallelIterator<Item = (FileId, &SourceFile)> {
		self.all_files().par_bridge()
	}

	pub(crate) fn set_dirty(&mut self, file_id: FileId) {
		self.dirty.insert(file_id);
	}

	pub(crate) fn update_global_symbols(&mut self, strings: &StringInterner) {
		let all_dirty = self.dirty.drain().collect::<Vec<_>>();

		for file_id in all_dirty {
			let symbols = std::ptr::addr_of_mut!(self.symbols);
			let sfile = self.get_file_mut(file_id).unwrap();
			let Some(parsed) = sfile.parsed.as_mut() else { continue; };

			for r in parsed.symbols.drain(..) {
				unsafe {
					(*symbols).remove(&r);
				}
			}

			let green = parsed.green.clone();
			let lang = sfile.lang;

			let contributed = match lang {
				LangId::ZScript => {
					let mut ctx = zscript::sema::UpdateContext {
						strings,
						project: self,
						file_id,
						contributed: vec![],
					};

					ctx.update(green);

					ctx.contributed
				}
				LangId::Unknown => continue,
			};

			let sfile = self.get_file_mut(file_id).unwrap();
			let Some(parsed) = sfile.parsed.as_mut() else { continue; };

			for iname in contributed {
				parsed.symbols.push(iname);
			}
		}
	}

	#[must_use]
	#[allow(unused)]
	pub(crate) fn globals(&self) -> &Scope {
		&self.symbols
	}

	#[must_use]
	pub(crate) fn globals_mut(&mut self) -> &mut Scope {
		&mut self.symbols
	}

	#[must_use]
	pub(crate) fn zscript(&self) -> &zscript::Storage {
		&self.zscript
	}

	#[must_use]
	pub(crate) fn zscript_mut(&mut self) -> &mut zscript::Storage {
		&mut self.zscript
	}

	pub(crate) fn on_file_delete(&mut self, path: PathBuf) {
		if let Some(file_id) = self.get_fileid(&path) {
			self.files.remove(&file_id);
			self.dirty.remove(&file_id);
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
				match zscript::rebuild_include_tree(self, de_path) {
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
}

#[derive(Debug)]
pub(crate) struct SourceFile {
	pub(crate) lang: LangId,
	pub(crate) text: String,
	pub(crate) lndx: LineIndex,
	pub(crate) parsed: Option<ParsedFile>,
}

impl SourceFile {
	#[must_use]
	pub(crate) fn parse_diagnostics(&self) -> Vec<Diagnostic> {
		let Some(parsed) = &self.parsed else { return vec![]; };

		match &parsed.errors {
			ParseErrors::ZScript(errors) => zscript::parse_errors_to_diags(self, errors),
		}
	}
}

#[derive(Debug)]
pub(crate) struct ParsedFile {
	pub(crate) green: GreenNode,
	/// Symbols "contributed" by this file to [`Project::symbols`], the global name
	/// resolution table. This is used to provide a "symbol delta" for whenever
	/// this file is changed, so that the entire global map does not have to be
	/// recomputed.
	pub(crate) symbols: Vec<IName>,
	pub(crate) errors: ParseErrors,
}

impl ParsedFile {
	#[must_use]
	pub(crate) fn token_at<L: LangExt>(
		&self,
		pos: lsp_types::Position,
		lndx: &LineIndex,
	) -> Option<SyntaxToken<L>> {
		let linecol = LineCol {
			line: pos.line,
			col: pos.character,
		};

		let Some(offs) = lndx.offset(linecol) else { return None; };

		SyntaxNode::<L>::new_root(self.green.clone())
			.token_at_offset(offs)
			.next()
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FileId(
	/// Corresponds to an element in [`PathInterner`].
	u32,
);

#[derive(Debug)]
pub enum ParseErrors {
	ZScript(Vec<ParseError<zscript::Syn>>),
}

/// Serves no special purpose; just ties together a [`FileId`] and [`TextSize`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FilePos {
	pub(crate) file: FileId,
	pub(crate) pos: TextSize,
}

#[derive(Debug)]
pub(crate) enum Datum {
	ZScript(zscript::Datum),
}

pub(crate) type ScopeStack<'s> = Vec<StackedScope<'s>>;

#[derive(Debug, Default)]
pub(crate) struct Scope {
	pub(crate) names: FxHashMap<IName, Datum>,
	pub(crate) addenda: Vec<IName>,
}

impl std::ops::Deref for Scope {
	type Target = FxHashMap<IName, Datum>;

	fn deref(&self) -> &Self::Target {
		&self.names
	}
}

impl std::ops::DerefMut for Scope {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.names
	}
}

#[derive(Debug)]
pub(crate) struct StackedScope<'s> {
	pub(crate) inner: &'s Scope,
	pub(crate) is_addendum: bool,
}

impl std::ops::Deref for StackedScope<'_> {
	type Target = Scope;

	fn deref(&self) -> &Self::Target {
		self.inner
	}
}

#[derive(Debug, Default)]
struct PathInterner {
	case: FxIndexSet<PathBuf>,
	nocase: FxIndexSet<ZPathBuf>,
}

impl PathInterner {
	fn intern(&mut self, p: PathBuf) -> FileId {
		let ret = self.case.insert_full(p.clone()).0;
		let z = self.nocase.insert_full(ZPathBuf::new(p)).0;
		debug_assert_eq!(ret, z);
		FileId(ret as u32)
	}
}
