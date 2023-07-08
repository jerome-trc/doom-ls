use std::path::{Path, PathBuf};

use doomfront::{
	rowan::{GreenNode, SyntaxNode, SyntaxToken, TextSize},
	LangExt,
};
use rustc_hash::{FxHashMap, FxHashSet};
use tracing::error;

use crate::{
	lines::{LineCol, LineIndex},
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
	symbols: FxHashMap<QName, SymbolKey>,
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
			symbols: FxHashMap::default(),
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
	pub(crate) fn lookup_global(&self, qname: &QName) -> Option<SymbolKey> {
		self.symbols.get(qname).copied()
	}

	pub(crate) fn set_dirty(&mut self, file_id: FileId) {
		self.dirty.insert(file_id);
	}

	pub(crate) fn update_global_symbols(&mut self) {
		let all_dirty = self.dirty.drain().collect::<Vec<_>>();

		for file_id in all_dirty {
			let sfile = self.get_file_mut(file_id).unwrap();
			let Some(parsed) = sfile.parsed.as_mut() else { continue; };

			let green = parsed.green.clone();

			match sfile.lang {
				LangId::Unknown => continue,
				LangId::ZScript => {
					let mut delta = SymbolDelta::default();

					for removed in parsed.symbols.drain(..) {
						let SymbolKey::ZScript(sym_k) = removed.1;
						delta.removed.push((removed.0, sym_k));
					}

					zscript::symbol_delta(&mut self.zscript, &mut delta, file_id, green);

					for removed in delta.removed {
						self.symbols.remove(&removed.0);
					}

					for added in delta.added {
						self.symbols.insert(added.0, SymbolKey::ZScript(added.1));
					}
				}
			};
		}
	}

	#[must_use]
	pub(crate) fn zscript(&self) -> &zscript::Storage {
		&self.zscript
	}

	pub(crate) fn on_file_delete(&mut self, path: PathBuf) {
		if let Some(file_id) = self.get_fileid(&path) {
			self.files.remove(&file_id);
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

#[derive(Debug)]
pub(crate) struct ParsedFile {
	pub(crate) green: GreenNode,
	/// Symbols "contributed" by this file to [`Project::symbols`], the global name
	/// resolution table. This is used to provide a "symbol delta" for whenever
	/// this file is changed, so that the entire global map does not have to be
	/// recomputed.
	pub(crate) symbols: Vec<(QName, SymbolKey)>,
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

/// A "qualified name", whose content is tagged with a namespace.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum QName {
	Type(Box<str>),
	Variable(Box<str>),
	// TODO: Symbolic constants, CVars, GLDEF objects, et cetera...
}

impl QName {
	#[must_use]
	pub(crate) fn for_type(string: &str) -> Self {
		Self::Type(string.to_owned().into_boxed_str())
	}

	#[must_use]
	pub(crate) fn for_var(string: &str) -> Self {
		Self::Variable(string.to_owned().into_boxed_str())
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FilePos {
	pub(crate) file: FileId,
	pub(crate) pos: TextSize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SymbolKey {
	ZScript(zscript::SymbolKey),
}

#[derive(Debug)]
pub(crate) struct SymbolDelta<K: Eq + Copy> {
	pub(crate) removed: Vec<(QName, K)>,
	pub(crate) added: Vec<(QName, K)>,
}

impl<K: Eq + Copy> Default for SymbolDelta<K> {
	fn default() -> Self {
		Self {
			removed: vec![],
			added: vec![],
		}
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
