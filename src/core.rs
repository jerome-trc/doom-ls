//! [`Core`], [`Project`], [`ReadyWorld`], [`PendingWorld`], [`WorkingWorld`], related.

use std::{
	hash::{BuildHasherDefault, Hash},
	path::{Path, PathBuf},
	time::Instant,
};

use crossbeam::atomic::AtomicCell;
use doomfront::{
	rowan::{
		cursor::{SyntaxNode, SyntaxToken},
		GreenNode, NodeOrToken, TextRange,
	},
	zdoom, LangExt,
};
use lsp_server::{Connection, Message, Notification};
use lsp_types::{
	notification::PublishDiagnostics, Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams,
	Location, PublishDiagnosticsParams, Url,
};
use parking_lot::{Mutex, RwLock, RwLockReadGuard, RwLockWriteGuard};
use rustc_hash::{FxHashMap, FxHasher};
use tracing::{debug, error};
use triomphe::Arc;

use crate::{
	arena::Arena,
	data::{
		Definition, FileSpan, InternalDb, ScopePtr, SymGraphKey, SymGraphVal, SymPtr, SymbolId,
		UserSymbol,
	},
	error::Error,
	intern::{NameInterner, NsName, PathInterner, PathIx},
	langs::{self, LangId},
	lines::{self, LineCol, LineIndex},
	util::{self, DiagBuilder},
	FxDashMap, FxDashView, FxHamt, UnitResult,
};

#[derive(Debug)]
pub(crate) struct Core {
	pub(crate) paths: Arc<PathInterner>,
	pub(crate) names: Arc<NameInterner>,
	pub(crate) ready: ReadyWorld,
	pub(crate) pending: PendingWorld,
	pub(crate) working: Arc<Mutex<WorkingWorld>>,
}

/// A conceptual "compilation unit" of sorts.
///
/// Represents one game modification (or standalone game) made to be loaded
/// to the user's source port of choice.
#[derive(Debug, Clone)]
pub(crate) struct Project {
	pub(crate) root: PathBuf,
	pub(crate) files: FxHamt<PathIx, Source>,
	pub(crate) zscript: langs::zscript::IncludeTree,
}

impl Core {
	#[must_use]
	pub(crate) fn project_with(&self, file_id: PathIx) -> Option<(usize, &Project)> {
		self.pending
			.projects
			.iter()
			.enumerate()
			.find_map(|(i, project)| project.files.get(&file_id).map(|_| (i, project)))
	}

	#[must_use]
	pub(crate) fn project_with_mut(&mut self, file_id: PathIx) -> Option<(usize, &mut Project)> {
		for (i, project) in self.pending.projects.iter_mut().enumerate() {
			if project.files.contains_key(&file_id) {
				return Some((i, project));
			}
		}

		None
	}

	#[must_use]
	pub(crate) fn project_by_child(&self, child_path: &Path) -> Option<(usize, &Project)> {
		self.pending
			.projects
			.iter()
			.enumerate()
			.find_map(|(i, project)| {
				util::path_is_child_of(&project.root, child_path).then_some((i, project))
			})
	}

	#[allow(unused)]
	#[must_use]
	pub(crate) fn project_by_child_mut(
		&mut self,
		child_path: &Path,
	) -> Option<(usize, &mut Project)> {
		for (i, project) in self.pending.projects.iter_mut().enumerate() {
			if util::path_is_child_of(&project.root, child_path) {
				return Some((i, project));
			}
		}

		None
	}

	#[must_use]
	pub(crate) fn file_with(&self, sym: &UserSymbol) -> &Source {
		self.ready.projects[sym.project as usize]
			.files
			.get(&sym.id.file_id)
			.unwrap()
	}

	#[allow(unused)]
	pub(crate) fn all_files(&self) -> impl Iterator<Item = &Source> {
		self.pending.projects.iter().flat_map(|p| p.files.values())
	}

	#[must_use]
	pub(crate) fn decl_text(&self, sym_ptr: &SymPtr, u_sym: &UserSymbol) -> Option<String> {
		match u_sym.def.as_ref() {
			None => None,
			Some(Definition::ZScript(datum)) => {
				Some(langs::zscript::help::decl_text(self, sym_ptr, u_sym, datum))
			}
			Some(Definition::_CVarInfo(_)) => unimplemented!(),
		}
	}

	#[must_use]
	pub(crate) fn make_location(&self, src: &Source, span: TextRange) -> Location {
		Location {
			uri: Url::from_file_path(self.paths.resolve(src.id)).unwrap(),
			range: src.make_range(span),
		}
	}

	pub(crate) fn reparse_dirty(&mut self, conn: &Connection) {
		if self.pending.dirty.is_empty() {
			return;
		}

		self.pending.sema_invalid = true;

		let paths = Arc::clone(&self.paths);
		let mut dirty = std::mem::take(&mut self.pending.dirty);

		let mut zs_vers_change = vec![];

		for (file_id, params) in dirty.drain() {
			let uri;

			let (project_ix, project) = self.project_with_mut(file_id).unwrap();
			let files = project.files.clone();
			let src = project.files.get_mut(&file_id).unwrap();

			let deltas = if let Some(p) = params {
				uri = p.text_document.uri;
				lines::splice_changes(&mut src.text, p.content_changes)
			} else {
				uri = Url::from_file_path(paths.resolve(file_id)).unwrap();
				None
			};

			// TODO:
			// - try reducing how many times the line index needs to be recomputed
			// - partial reparsing
			// - DECORATE include tree changes

			src.lines = LineIndex::new(&src.text);

			let (green, mut diags) = if let Some(_deltas) = deltas {
				match src.lang {
					LangId::Unknown => continue,
					LangId::CVarInfo | LangId::Decorate => unimplemented!(),
					LangId::ZScript => langs::zscript::parse::full(src, project.zscript.version),
				}
			} else {
				match src.lang {
					LangId::Unknown => continue,
					LangId::CVarInfo | LangId::Decorate => unimplemented!(),
					LangId::ZScript => langs::zscript::parse::full(src, project.zscript.version),
				}
			};

			src.green = Some(green);

			if project.zscript.root.is_some_and(|r| r == file_id) {
				for prev_nb in project.zscript.invalidate_includer(file_id) {
					if project.zscript.includes.neighbors(prev_nb).next().is_some() {
						continue;
					}

					let nb_uri = Url::from_file_path(paths.resolve(prev_nb)).unwrap();
					util::clear_diags(conn, nb_uri);
				}

				let prev_vers = project.zscript.version;

				match langs::zscript::parse::resolve_version(src) {
					Ok(vers) => project.zscript.version = vers,
					Err(diag) => diags.push(diag),
				}

				if project.zscript.version != prev_vers {
					zs_vers_change.push(project_ix);
				}

				let graph = &mut project.zscript.includes;

				langs::zscript::inctree::get_includes(
					paths.as_ref(),
					&project.root,
					files,
					graph,
					src,
					&mut diags,
				);
			} else if project.zscript.includes.contains_node(file_id) {
				for prev_nb in project.zscript.invalidate_includer(file_id) {
					if project.zscript.includes.neighbors(prev_nb).next().is_some() {
						continue;
					}

					let nb_uri = Url::from_file_path(paths.resolve(prev_nb)).unwrap();
					util::clear_diags(conn, nb_uri);
				}

				let graph = &mut project.zscript.includes;

				langs::zscript::inctree::get_includes(
					paths.as_ref(),
					&project.root,
					files,
					graph,
					src,
					&mut diags,
				);
			}

			if !diags.is_empty() {
				let _ = self.pending.malformed.insert(file_id, diags.clone());
			} else {
				let _ = self.pending.malformed.remove(&file_id);
			}

			util::send_diags(conn, uri, diags);
		}

		self.pending.dirty = dirty;

		for v in zs_vers_change {
			let project = &self.pending.projects[v];

			for src in project.zscript.files(project) {
				self.pending.dirty.insert(src.id, None);
			}
		}
	}

	pub(crate) fn on_file_create(&mut self, path: PathBuf) -> UnitResult {
		let file_id = self.paths.intern(&path);

		let Some((i, _)) = self.project_by_child(&path) else {
			return Ok(());
		};

		let text = std::fs::read_to_string(&path).map_err(Error::from)?;
		let lines = LineIndex::new(&text);

		let mut src = Source {
			id: file_id,
			lang: LangId::Unknown,
			text,
			green: None,
			lines,
		};

		let src = if self.pending.projects[i]
			.zscript
			.includes
			.contains_node(file_id)
		{
			// TODO: similar handling will need to be applied to the DECORATE root,
			// and other such special root-directory files identifiable by their path stem.
			src.lang = LangId::ZScript;
			self.parse_new_file(i, src)
		} else {
			src
		};

		self.pending.dirty.insert(file_id, None);
		self.pending.projects[i].files.insert(file_id, src.clone());

		Ok(())
	}

	pub(crate) fn parse_new_file(&mut self, project_ix: usize, mut src: Source) -> Source {
		let (green, diags) = match src.lang {
			LangId::ZScript => {
				let vers = self.pending.projects[project_ix].zscript.version;
				langs::zscript::parse::full(&src, vers)
			}
			LangId::CVarInfo | LangId::Decorate => return src, // TODO
			LangId::Unknown => return src,
		};

		src.green = Some(green);

		if !diags.is_empty() {
			self.pending.malformed.insert(src.id, diags);
		}

		src
	}

	pub(crate) fn on_file_delete(&mut self, conn: &Connection, file_id: PathIx) {
		let _ = self.pending.dirty.remove(&file_id);
		let _ = self.pending.malformed.remove(&file_id);

		let Some((i, _)) = self.project_with(file_id) else {
			return;
		};

		self.pending.projects[i].files.remove(&file_id);
		let project = &mut self.pending.projects[i];

		{
			if project.zscript.root.is_some_and(|r| r == file_id) {
				project.zscript.root = None;
				project.zscript.includes.clear();
				project.zscript.version = zdoom::Version::V2_4_0;
			}

			if project.zscript.root.is_none() {
				if let Ok(root_reader) = util::root_dir_reader(conn, &project.root) {
					for p in root_reader {
						let Some(fstem) = p.file_stem() else {
							continue;
						};

						if fstem.eq_ignore_ascii_case("zscript") {
							project.zscript.root = Some(self.paths.intern(&p));
						}
					}
				}
			}
		}

		self.pending.sema_invalid = true;
	}

	#[must_use]
	pub(crate) fn should_refresh(&self) -> bool {
		!self.pending.projects.is_empty()
			&& self.pending.sema_invalid
			&& self.pending.malformed.is_empty()
	}

	#[must_use]
	pub(crate) fn finish_refresh(&mut self, conn: &Connection) -> bool {
		let Some(mut working) = self.working.try_lock() else {
			// The worker thread is not done with the working world yet.
			// Serve another LSP message before trying again.
			return false;
		};

		debug!("Updating ready world.");

		std::mem::swap(&mut self.ready.arena, &mut working.arena);

		self.ready.globals.clear();
		self.ready.projects = std::mem::take(&mut working.projects);

		for rwlock in working.globals.drain(..) {
			self.ready.globals.push(rwlock.into_inner());
		}

		fn swap_dashmap_with_readonly<K, V>(
			ready: &mut FxDashView<K, V>,
			working: &mut FxDashMap<K, V>,
		) where
			K: Eq + Hash + Clone,
		{
			let mut temp =
				std::mem::replace(ready, FxDashMap::default().into_read_only()).into_inner();
			std::mem::swap(&mut temp, working);
			*ready = temp.into_read_only();
		}

		swap_dashmap_with_readonly(&mut self.ready.sym_graph, &mut working.sym_graph);
		swap_dashmap_with_readonly(&mut self.ready.symbols, &mut working.symbols);
		swap_dashmap_with_readonly(&mut self.ready.scopes, &mut working.scopes);

		let diags = std::mem::take(&mut working.diags);

		for (file_id, diags) in diags {
			let path = self.paths.resolve(file_id);
			let uri = Url::from_file_path(path).unwrap();

			let result = conn.sender.send(Message::Notification(Notification {
				method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
					.to_string(),
				params: serde_json::to_value(PublishDiagnosticsParams {
					uri,
					diagnostics: diags,
					version: None,
				})
				.unwrap(),
			}));

			if let Err(err) = result {
				error!("Failed to send sema. diagnostics: {err}");
			}
		}

		debug!("Ready world is up-to-date.");
		true
	}
}

impl Default for Core {
	fn default() -> Self {
		let names = Arc::new(NameInterner::default());
		let paths = Arc::new(PathInterner::default());
		let internal = Arc::new(InternalDb::new(&names));

		Self {
			paths: paths.clone(),
			names: names.clone(),
			ready: ReadyWorld {
				arena: Arena::default(),
				projects: vec![],
				globals: vec![],
				symbols: FxDashMap::default().into_read_only(),
				scopes: FxDashMap::default().into_read_only(),
				sym_graph: FxDashMap::default().into_read_only(),
				internal: internal.clone(),
			},
			pending: PendingWorld {
				projects: vec![],
				malformed: FxHashMap::default(),
				dirty: FxHashMap::default(),
				last_change: Instant::now(),
				sema_invalid: true,
			},
			working: Arc::new(Mutex::new(WorkingWorld {
				arena: Arena::default(),
				paths,
				names,
				internal,
				projects: vec![],

				globals: vec![],
				diags: FxDashMap::default(),
				symbols: FxDashMap::default(),
				scopes: FxDashMap::default(),
				sym_graph: FxDashMap::default(),
			})),
		}
	}
}

#[derive(Debug)]
pub(crate) struct ReadyWorld {
	pub(crate) arena: Arena,
	#[allow(unused)]
	pub(crate) internal: Arc<InternalDb>,
	pub(crate) projects: Vec<Project>,
	/// Runs parallel to `projects`.
	pub(crate) globals: Vec<Scope>,
	pub(crate) symbols: FxDashView<SymbolId, SymPtr>,
	/// For scopes that aren't attached to symbols. At the moment,
	/// this exclusively means function bodies (ZScript, DECORATE, ACS),
	/// and ACS module scopes.
	pub(crate) scopes: FxDashView<FileSpan, ScopePtr>,
	pub(crate) sym_graph: FxDashView<SymGraphKey, SymGraphVal>,
}

#[derive(Debug)]
pub(crate) struct PendingWorld {
	pub(crate) projects: Vec<Project>,
	/// Any time a file is re-parsed:
	/// - its ID is inserted if it has any parse errors
	/// - its ID is removed if it has no parse errors
	/// Pending will not be sent to Working unless this map is empty.
	pub(crate) malformed: FxHashMap<PathIx, Vec<Diagnostic>>,
	/// Files that have had text content changes and need to be re-parsed.
	pub(crate) dirty: FxHashMap<PathIx, Option<DidChangeTextDocumentParams>>,
	/// Every time a `textDocument/didChange` notification arrives, this is updated
	/// to the current time. The dirty file list is not re-parsed until this
	/// a certain amount of time has elapsed since this, since there's no use
	/// in continually re-parsing if the user is still typing at full speed.
	pub(crate) last_change: Instant,
	/// Set to `true` when clearing `self.dirty`; set back to `false`
	/// when a refresh starts.
	pub(crate) sema_invalid: bool,
}

#[derive(Debug)]
pub(crate) struct WorkingWorld {
	pub(crate) arena: Arena,
	// Context /////////////////////////////////////////////////////////////////
	pub(crate) paths: Arc<PathInterner>,
	pub(crate) names: Arc<NameInterner>,
	pub(crate) internal: Arc<InternalDb>,
	pub(crate) projects: Vec<Project>,
	// Artefacts ///////////////////////////////////////////////////////////////
	pub(crate) globals: Vec<RwLock<Scope>>,
	pub(crate) diags: FxDashMap<PathIx, Vec<Diagnostic>>,
	/// The source of truth for user symbol data.
	/// Arena pointers should only be freed through here.
	pub(crate) symbols: FxDashMap<SymbolId, SymPtr>,
	/// See [`ReadyWorld::scopes`] for details.
	pub(crate) scopes: FxDashMap<FileSpan, ScopePtr>,
	pub(crate) sym_graph: FxDashMap<SymGraphKey, SymGraphVal>,
}

impl WorkingWorld {
	pub(crate) fn refresh(&mut self, ctx: WorkContext) {
		debug_assert!(self.diags.is_empty());
		debug_assert!(self.globals.is_empty());

		self.projects = ctx.projects;

		// TODO:
		// - at what scale does parallel iteration become faster than serial`?
		// - would clearing these structures over multiple threads be faster?
		self.symbols.iter().for_each(|kvp| unsafe {
			if let Some(sym_ptr) = kvp.value().as_ptr() {
				std::ptr::drop_in_place(sym_ptr.as_ptr());
			}
		});

		self.symbols.clear();
		self.scopes.clear();
		self.sym_graph.clear();
		self.arena.reset();

		self.globals.push(RwLock::new(self.internal.global.clone()));

		for (i, project) in self.projects.iter().enumerate() {
			// Declare global symbols.
			debug_assert_eq!(ctx.tracker.phase.load(), WorkPhase::Declaration);

			if project.zscript.root.is_some() {
				langs::zscript::front1(self, i, project);
			}

			#[cfg(debug_assertions)]
			debug!(
				"Finished declaring symbols for project: {}",
				project.root.display()
			);

			// Defining and checking types.
			ctx.tracker.phase.store(WorkPhase::Definition);

			if project.zscript.root.is_some() {
				langs::zscript::front2(self, i, project);
			}

			#[cfg(debug_assertions)]
			debug!("Finished refreshing project: {}", project.root.display());

			if (i + 1) == self.projects.len() {
				break;
			}

			let this_g = self.globals[i].get_mut().clone();
			self.globals.push(RwLock::new(this_g));
		}
	}

	pub(crate) fn global_scope(&self, project_ix: usize) -> RwLockReadGuard<Scope> {
		self.globals[project_ix].read()
	}

	pub(crate) fn global_scope_mut(&self, project_ix: usize) -> RwLockWriteGuard<Scope> {
		self.globals[project_ix].write()
	}

	#[allow(unused)]
	#[must_use]
	pub(crate) fn get_file(&self, project_ix: usize, fspan: FileSpan) -> &Source {
		self.projects[project_ix].files.get(&fspan.file_id).unwrap()
	}

	#[must_use]
	pub(crate) fn file_with(&self, sym: &UserSymbol) -> &Source {
		self.projects[sym.project as usize]
			.files
			.get(&sym.id.file_id)
			.unwrap()
	}
}

#[derive(Debug, Clone)]
pub(crate) struct Source {
	pub(crate) id: PathIx,
	pub(crate) lang: LangId,
	pub(crate) text: String,
	pub(crate) lines: LineIndex,
	pub(crate) green: Option<GreenNode>,
}

impl Source {
	#[must_use]
	pub(crate) fn make_range(&self, span: TextRange) -> lsp_types::Range {
		lsp_types::Range {
			start: lsp_types::Position::from(self.lines.line_col(span.start())),
			end: lsp_types::Position::from(self.lines.line_col(span.end())),
		}
	}

	#[must_use]
	pub(crate) fn token_at(&self, pos: lsp_types::Position) -> Option<SyntaxToken> {
		let Some(green) = self.green.as_ref() else {
			return None;
		};

		let linecol = LineCol {
			line: pos.line,
			col: pos.character,
		};

		let Some(offs) = self.lines.offset(linecol) else {
			return None;
		};

		SyntaxNode::new_root(green.clone())
			.token_at_offset(offs)
			.next()
	}

	#[must_use]
	pub(crate) fn node_covering<L: LangExt>(
		&self,
		span: TextRange,
	) -> doomfront::rowan::SyntaxNode<L> {
		let file_node =
			doomfront::rowan::SyntaxNode::new_root(self.green.as_ref().unwrap().clone());
		let sym_elem = file_node.covering_element(span);

		match sym_elem {
			NodeOrToken::Node(n) => n,
			NodeOrToken::Token(t) => t.parent().unwrap(),
		}
	}

	#[must_use]
	pub(crate) fn diag_builder(
		&self,
		span: TextRange,
		severity: DiagnosticSeverity,
		message: String,
	) -> DiagBuilder {
		DiagBuilder(Diagnostic {
			range: self.make_range(span),
			severity: Some(severity),
			code: None,
			code_description: None,
			source: Some("doomls".to_string()),
			message,
			related_information: None,
			tags: None,
			data: None,
		})
	}
}

#[derive(Debug)]
pub(crate) struct WorkContext {
	pub(crate) tracker: Arc<WorkTracker>,
	pub(crate) projects: Vec<Project>,
}

#[derive(Debug)]
pub(crate) struct WorkTracker {
	pub(crate) phase: AtomicCell<WorkPhase>,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WorkPhase {
	#[default]
	Declaration,
	Definition,
	// TODO: this will be used to signal to the main thread whether it should wait
	// for a refresh to complete or service user requests from the existing ready world.
}

pub(crate) type Scope = im::HashMap<NsName, SymPtr, BuildHasherDefault<FxHasher>>;

const _STATIC_ASSERT_WORKPHASE_LOCKFREE: () = {
	assert!(AtomicCell::<WorkPhase>::is_lock_free());
};
