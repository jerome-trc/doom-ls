use std::{
	path::{Path, PathBuf},
	rc::Rc,
};

use doomfront::{
	rowan::{GreenNode, SyntaxNode, SyntaxToken, TextRange},
	LangExt, ParseError,
};
use lsp_types::Diagnostic;
use rayon::prelude::{ParallelBridge, ParallelIterator};
use rustc_hash::{FxHashMap, FxHashSet};
use tracing::error;

use crate::{
	lines::{LineCol, LineIndex},
	names::{IName, StringInterner},
	paths::{FileId, PathInterner},
	zscript, LangId,
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
	symbols: Rc<Scope>,
}

impl Project {
	#[must_use]
	pub(crate) fn new(root: PathBuf) -> Self {
		Self {
			root,
			paths: PathInterner::default(),
			files: FxHashMap::default(),
			dirty: FxHashSet::default(),
			symbols: Rc::<Scope>::default(),
		}
	}

	#[must_use]
	pub(crate) fn root(&self) -> &Path {
		&self.root
	}

	/// Note that this returns `None` if no source has been registered under
	/// the file ID corresponding to `path` yet.
	#[must_use]
	pub(crate) fn get_fileid(&self, path: &Path) -> Option<FileId> {
		self.paths()
			.get_native(path)
			.filter(|file_id| self.files.contains_key(file_id))
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
	pub(crate) fn lookup_global(&self, iname: IName) -> Option<&Datum> {
		self.symbols.get(&iname)
	}

	pub(crate) fn all_files(&self) -> impl Iterator<Item = (FileId, &SourceFile)> {
		self.files.iter().map(|t| (*t.0, t.1))
	}

	#[must_use = "iterators are lazy and do nothing unless consumed"]
	pub(crate) fn all_files_par(&self) -> impl ParallelIterator<Item = (FileId, &SourceFile)> {
		self.all_files().par_bridge()
	}

	#[must_use]
	#[allow(unused)]
	pub(crate) fn globals(&self) -> &Scope {
		&self.symbols
	}

	#[must_use]
	pub(crate) fn globals_mut(&mut self) -> &mut Scope {
		Rc::get_mut(&mut self.symbols).unwrap()
	}

	pub(crate) fn scope(&self) -> &Rc<Scope> {
		&self.symbols
	}

	#[must_use]
	pub(crate) fn paths(&self) -> &PathInterner {
		&self.paths
	}

	#[must_use]
	pub(crate) fn paths_mut(&mut self) -> &mut PathInterner {
		&mut self.paths
	}

	pub(crate) fn set_dirty(&mut self, file_id: FileId) {
		self.dirty.insert(file_id);
	}

	pub(crate) fn on_file_delete(&mut self, path: PathBuf) {
		if let Some(file_id) = self.get_fileid(&path) {
			self.files.remove(&file_id);
			self.dirty.remove(&file_id);
		}
	}

	pub(crate) fn update_global_symbols(&mut self, strings: &StringInterner) {
		let all_dirty = self.dirty.drain().collect::<Vec<_>>();
		let symbols = Rc::get_mut(&mut self.symbols).unwrap() as *mut Scope;

		for file_id in all_dirty {
			let sfile = self.get_file_mut(file_id).unwrap();
			let Some(parsed) = sfile.parsed.as_mut() else { continue; };

			for r in parsed.symbols.drain(..) {
				unsafe {
					(*symbols).remove(&r);
				}
			}

			let green: GreenNode = parsed.green.clone();
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

#[derive(Debug)]
pub(crate) enum ParseErrors {
	ZScript(Vec<ParseError<zscript::Syn>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct DatumPos {
	pub(crate) file: FileId,
	pub(crate) full_range: TextRange,
	pub(crate) name_range: TextRange,
}

#[derive(Debug)]
pub(crate) enum Datum {
	ZScript(zscript::Datum),
}

impl Datum {
	#[must_use]
	pub(crate) fn pos(&self) -> Option<DatumPos> {
		match self {
			Self::ZScript(dat_zs) => dat_zs.pos(),
		}
	}

	pub(crate) fn add_scopes_containing(&self, scopes: &mut Vec<StackedScope>, range: TextRange) {
		match self {
			Self::ZScript(dat_zs) => {
				dat_zs.add_scopes_containing(scopes, range);
			}
		}
	}
}

pub(crate) type Scope = FxHashMap<IName, Datum>;

#[derive(Debug)]
pub(crate) struct StackedScope {
	pub(crate) ix_project: Option<usize>,
	pub(crate) inner: Rc<Scope>,
	pub(crate) is_addendum: bool,
}
