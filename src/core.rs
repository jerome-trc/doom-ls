use std::{
	hash::BuildHasherDefault,
	path::{Path, PathBuf},
	time::Instant,
};

use append_only_vec::AppendOnlyVec;
use crossbeam::{atomic::AtomicCell, channel::Receiver};
use doomfront::{
	rowan::{
		cursor::{SyntaxNode, SyntaxToken},
		GreenNode, NodeOrToken, SyntaxKind, TextRange,
	},
	zdoom, LangExt,
};
use lsp_server::{Connection, Message, Notification};
use lsp_types::{
	notification::PublishDiagnostics, Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams,
	OneOf, PublishDiagnosticsParams, Url,
};
use parking_lot::{Mutex, RwLock, RwLockReadGuard, RwLockWriteGuard};
use rustc_hash::{FxHashMap, FxHasher};
use tracing::{debug, error};
use triomphe::Arc;

use crate::{
	intern::{NameInterner, NsName, PathInterner, PathIx},
	langs::{self, LangId},
	lines::{self, LineCol, LineIndex},
	util::{self, DiagBuilder},
	FxDashMap, FxDashView, FxHamt,
};

#[derive(Debug)]
pub(crate) struct Core {
	pub(crate) paths: Arc<PathInterner>,
	pub(crate) ready: ReadyWorld,
	pub(crate) pending: PendingWorld,
	pub(crate) working: Arc<Mutex<WorkingWorld>>,
}

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

	#[must_use]
	#[allow(unused)]
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

	pub(crate) fn reparse_dirty(&mut self, conn: &Connection) {
		if self.pending.dirty.is_empty() {
			return;
		}

		let mut dirty = std::mem::take(&mut self.pending.dirty);

		self.pending.sema_invalid = true;

		for (file_id, params) in dirty.drain() {
			let (_, project) = self.project_with_mut(file_id).unwrap();
			let src = project.files.get_mut(&file_id).unwrap();

			let deltas = lines::splice_changes(&mut src.text, params.content_changes);
			// TODO:
			// - try reducing how many times the line index needs to be recomputed
			// - partial reparsing
			// - include tree changes
			// - ZScript version changes
			src.lines = LineIndex::new(&src.text);

			let (green, diags) = if let Some(_deltas) = deltas {
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

			if !diags.is_empty() {
				let _ = self.pending.malformed.insert(file_id, diags.clone());
			} else {
				let _ = self.pending.malformed.remove(&file_id);
			}

			let result = conn.sender.send(Message::Notification(Notification {
				method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
					.to_string(),
				params: serde_json::to_value(PublishDiagnosticsParams {
					uri: params.text_document.uri,
					diagnostics: diags,
					version: None,
				})
				.unwrap(),
			}));

			if let Err(err) = result {
				error!("Failed to send a diagnostic: {err}");
			}
		}

		self.pending.dirty = dirty;
	}

	pub(crate) fn add_file(&mut self, project_ix: usize, mut src: Source) -> Source {
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

	pub(crate) fn on_file_delete(&mut self, file_id: PathIx) {
		let _ = self.pending.dirty.remove(&file_id);
		let _ = self.pending.malformed.remove(&file_id);

		let Some((i, _)) = self.project_with(file_id) else {
			return;
		};

		self.pending.projects[i].files.remove(&file_id);

		let project = &mut self.pending.projects[i];

		if project.zscript.root.is_some_and(|r| r == file_id) {
			project.zscript.root = None;
			project.zscript.includes.clear();
			project.zscript.version = zdoom::Version::V2_4_0;
		} else if let Some(_) = project.zscript.includes.get(&file_id).copied() {
			// TODO
		}
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

		self.ready.globals.clear();
		self.ready.projects = std::mem::take(&mut working.projects);
		self.ready.decls = std::mem::replace(&mut working.decls, AppendOnlyVec::new());
		self.ready.defs = std::mem::replace(&mut working.defs, AppendOnlyVec::new());

		for rwlock in working.globals.drain(..) {
			self.ready.globals.push(rwlock.into_inner());
		}

		{
			let mut sym_graph = std::mem::replace(
				&mut self.ready.sym_graph,
				FxDashMap::default().into_read_only(),
			)
			.into_inner();
			std::mem::swap(&mut sym_graph, &mut working.sym_graph);
			self.ready.sym_graph = sym_graph.into_read_only();
		}

		{
			let mut scopes = std::mem::replace(
				&mut self.ready.scopes,
				FxDashMap::default().into_read_only(),
			)
			.into_inner();
			std::mem::swap(&mut scopes, &mut working.scopes);
			self.ready.scopes = scopes.into_read_only();
		}

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
		let names = NameInterner::default();
		let paths = Arc::new(PathInterner::default());
		let internal = Arc::new(InternalDb::new(&names));

		Self {
			paths: paths.clone(),
			ready: ReadyWorld {
				projects: vec![],
				decls: AppendOnlyVec::new(),
				globals: vec![],
				scopes: FxDashMap::default().into_read_only(),
				defs: AppendOnlyVec::new(),
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
				paths,
				names,
				internal,
				projects: vec![],
				scope_work: FxDashMap::default(),
				extensions: AppendOnlyVec::new(),
				decls: AppendOnlyVec::new(),
				defs: AppendOnlyVec::new(),
				globals: vec![],
				scopes: FxDashMap::default(),
				sym_graph: FxDashMap::default(),
				diags: FxDashMap::default(),
			})),
		}
	}
}

#[derive(Debug)]
pub(crate) struct InternalDb {
	pub(crate) global: Scope,
	pub(crate) symbols: Vec<InternalSymbol>,
	// Reserve the indices of symbols which are referenced often
	// to avoid having to look them up.
	pub(crate) ixs_zscript: langs::zscript::internal::Indices,
}

impl InternalDb {
	#[must_use]
	pub(crate) fn new(names: &NameInterner) -> Self {
		let start_time = Instant::now();

		let mut globals = Scope::default();
		let mut symbols = vec![];
		let ixs_zscript = langs::zscript::internal::register(names, &mut globals, &mut symbols);

		debug!(
			"Internal symbol database initialized in {}ms.",
			start_time.elapsed().as_millis()
		);

		Self {
			global: globals,
			symbols,
			ixs_zscript,
		}
	}

	#[must_use]
	pub(crate) fn by_index(&self, index: usize) -> &InternalSymbol {
		&self.symbols[index]
	}
}

#[derive(Debug)]
pub(crate) struct InternalSymbol {
	pub(crate) lang: LangId,
	pub(crate) decl: &'static str,
	pub(crate) docs: &'static [&'static str],
	pub(crate) scope: Scope,
	pub(crate) def: Definition,
}

#[derive(Debug)]
pub(crate) struct ReadyWorld {
	pub(crate) projects: Vec<Project>,
	pub(crate) decls: AppendOnlyVec<Symbol>,
	/// Runs parallel to `projects`.
	pub(crate) globals: Vec<Scope>,
	/// If a scope within corresponds to a symbol (e.g. a class), the key `FileSpan`
	/// will be equivalent to [`Symbol::id`] for that symbol. If the scope corresponds
	/// to a block, the `FileSpan` will encompass the whole block.
	pub(crate) scopes: FxDashView<FileSpan, Scope>,
	pub(crate) defs: AppendOnlyVec<Definition>,
	pub(crate) sym_graph: FxDashView<SymGraphKey, SymGraphValue>,
	pub(crate) internal: Arc<InternalDb>,
}

impl ReadyWorld {
	#[must_use]
	pub(crate) fn symbol(&self, sym_ix: SymIx) -> OneOf<&Symbol, &InternalSymbol> {
		if sym_ix.0.is_positive() {
			OneOf::Left(&self.decls[sym_ix.0 as usize])
		} else {
			OneOf::Right(&self.internal.symbols[sym_ix.0.unsigned_abs() as usize])
		}
	}

	#[must_use]
	pub(crate) fn definition(&self, def_ix: DefIx) -> &Definition {
		&self.defs[def_ix.0 as usize]
	}
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
	pub(crate) dirty: FxHashMap<PathIx, DidChangeTextDocumentParams>,
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
	// Context /////////////////////////////////////////////////////////////////
	pub(crate) paths: Arc<PathInterner>,
	pub(crate) names: NameInterner,
	pub(crate) internal: Arc<InternalDb>,
	pub(crate) projects: Vec<Project>,
	// Intermediate state //////////////////////////////////////////////////////
	pub(crate) scope_work: FxDashMap<FileSpan, Receiver<Scope>>,
	pub(crate) extensions: AppendOnlyVec<FileSpan>,
	// Artefacts ///////////////////////////////////////////////////////////////
	pub(crate) decls: AppendOnlyVec<Symbol>,
	pub(crate) defs: AppendOnlyVec<Definition>,
	pub(crate) globals: Vec<RwLock<Scope>>,
	/// See [`ReadyWorld::scopes`] for details.
	pub(crate) scopes: FxDashMap<FileSpan, Scope>,
	pub(crate) sym_graph: FxDashMap<SymGraphKey, SymGraphValue>,
	pub(crate) diags: FxDashMap<PathIx, Vec<Diagnostic>>,
}

impl WorkingWorld {
	#[must_use]
	pub(crate) fn symbol(&self, sym_ix: SymIx) -> OneOf<&Symbol, &InternalSymbol> {
		if sym_ix.0.is_positive() || sym_ix.0 == 0 {
			OneOf::Left(&self.decls[sym_ix.0 as usize])
		} else {
			OneOf::Right(&self.internal.symbols[sym_ix.0.unsigned_abs() as usize])
		}
	}

	pub(crate) fn refresh(&mut self, ctx: WorkContext) {
		self.projects = ctx.projects;

		self.extensions = AppendOnlyVec::new();
		// TODO: Would it be faster to clear these maps over separate threads?
		self.scopes.clear();
		self.sym_graph.clear();
		self.scope_work.clear();

		debug_assert_eq!(self.decls.len(), 0);
		debug_assert_eq!(self.defs.len(), 0);
		debug_assert!(self.diags.is_empty());
		debug_assert!(self.globals.is_empty());

		self.globals.push(RwLock::new(self.internal.global.clone()));

		for (i, project) in self.projects.iter().enumerate() {
			// Declare global symbols.
			debug_assert_eq!(ctx.tracker.phase.load(), WorkPhase::Declaration);

			if project.zscript.root.is_some() {
				langs::zscript::front1(self, i, project);
			}

			#[cfg(debug_assertions)]
			debug!("Finished phase 1 for project: {}", project.root.display());

			// Fill out scopes of global symbols (e.g. class fields, enum variants).
			// Resolve ZScript and DECORATE inheritance; expand ZScript mixins.
			ctx.tracker.phase.store(WorkPhase::PostDeclaration);

			if project.zscript.root.is_some() {
				langs::zscript::front2(self, i, project);
				langs::zscript::extend_classes_and_structs(self, i);
			}

			#[cfg(debug_assertions)]
			debug!("Finished phase 2 for project: {}", project.root.display());

			// Defining and checking types.
			ctx.tracker.phase.store(WorkPhase::Definition);

			// Verify nothing unexpected happened during previous two phases.
			debug_assert_eq!(self.defs.len(), 0);

			if project.zscript.root.is_some() {
				langs::zscript::front3(self, i, project);
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

	#[must_use]
	pub(crate) fn get_file(&self, project_ix: usize, fspan: FileSpan) -> &Source {
		self.projects[project_ix].files.get(&fspan.file_id).unwrap()
	}

	#[must_use]
	pub(crate) fn file_with(&self, sym: &Symbol) -> &Source {
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
	PostDeclaration,
	Definition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FileSpan {
	pub(crate) file_id: PathIx,
	pub(crate) span: TextRange,
}

impl PartialOrd for FileSpan {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		match self.file_id.partial_cmp(&other.file_id) {
			Some(core::cmp::Ordering::Equal) => {}
			ord => return ord,
		}

		PartialOrd::partial_cmp(
			&(u32::from(self.span.start()), u32::from(self.span.end())),
			&(u32::from(other.span.start()), u32::from(other.span.end())),
		)
	}
}

impl Ord for FileSpan {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		match self.file_id.cmp(&other.file_id) {
			core::cmp::Ordering::Equal => {}
			ord => return ord,
		}

		Ord::cmp(
			&(u32::from(self.span.start()), u32::from(self.span.end())),
			&(u32::from(other.span.start()), u32::from(other.span.end())),
		)
	}
}

/// To mitigate chances of misusing [`FileSpan`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub(crate) struct SymbolId(pub(crate) FileSpan);

impl std::ops::Deref for SymbolId {
	type Target = FileSpan;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

/// A strongly-typed index into:
/// - [`ReadyWorld::decls`]/[`WorkingWorld::decls`], if positive.
/// - [`InternalDb::symbols`] if negative (via `abs`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct SymIx(pub(crate) i32);

impl SymIx {
	#[must_use]
	pub(crate) fn internal(index: usize) -> Self {
		Self(-(index as i32))
	}
}

/// A strongly-typed index into [`ReadyWorld::defs`]/[`WorkingWorld::defs`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct DefIx(pub(crate) u32);

impl DefIx {
	pub(crate) const PLACEHOLDER: Self = Self(u32::MAX);
	pub(crate) const PENDING: Self = Self(u32::MAX - 1);
}

#[derive(Debug)]
pub(crate) struct Symbol {
	pub(crate) id: SymbolId,
	pub(crate) lang: LangId,
	pub(crate) syn: SyntaxKind,
	/// An index into [`WorkingWorld::projects`].
	pub(crate) project: u8,
	/// The part of this symbol that's important to serving diagnostics.
	///
	/// For example, a ZScript function definition's "critical span" starts at its
	/// first qualifier keyword or return type token and ends at the closing
	/// parenthesis of its parameter list or `const` keyword.
	pub(crate) crit_span: TextRange,
	pub(crate) def: AtomicCell<DefIx>,
}

impl Symbol {
	#[must_use]
	pub(crate) fn definition(&self) -> Option<DefIx> {
		let ret = self.def.load();
		(ret != DefIx::PLACEHOLDER).then_some(ret)
	}
}

#[derive(Debug)]
pub(crate) enum Definition {
	CVarInfo(langs::cvarinfo::sema::Datum),
	ZScript(langs::zscript::sema::Datum),
}

#[derive(Debug)]
pub(crate) enum SymGraphValue {
	Symbol(SymIx),
	Symbols(Vec<SymIx>),
	References(Vec<FileSpan>),
}

/// A key into [`ReadyWorld::sym_graph`]/[`WorkingWorld::sym_graph`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SymGraphKey {
	/// The value is the direct ancestor of a ZScript or DECORATE class.
	ParentOf(SymIx),
	/// The value is the set of all ancestors of a ZScript or DECORATE class.
	ChildrenOf(SymIx),

	/// The value is the set of all mixin classes applied to a ZScript class.
	Mixins(SymIx),
	/// The value is the set of all ZScript classes which apply a mixin.
	MixinRefs(SymIx),

	/// The value is the set of all members of a ZScript or DECORATE aggregate.
	Members(SymIx),
	/// The value is the ZScript or DECORATE aggregate which has this symbol as a member.
	Holder(SymIx),

	/// The value is the ZScript abstract or virtual function.
	PrototypeFor(SymIx),
	/// The value is the set of all overrides of this ZScript abstract or virtual function.
	OverrideOf(SymIx),

	/// The value is the symbol referred to by the token at this file-span.
	/// This span does not necessarily have to map to a token; for example,
	/// a ZScript string literal may have multiple LANGUAGE ID references in it.
	Reference(FileSpan),
	/// The value is a [`SymGraphValue::References`]; all spans referring to this symbol.
	/// These spans do not necessarily have to each map to a token; for example,
	/// a ZScript string literal may have multiple LANGUAGE ID references in it.
	Referred(SymIx),
}

pub(crate) type Scope = im::HashMap<NsName, SymIx, BuildHasherDefault<FxHasher>>;

const _STATIC_ASSERT_WORKPHASE_LOCKFREE: () = {
	assert!(AtomicCell::<WorkPhase>::is_lock_free());
};

const _STATIC_ASSERT_DEFIX_LOCKFREE: () = {
	assert!(AtomicCell::<DefIx>::is_lock_free());
};
