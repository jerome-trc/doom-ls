//! # DoomLS
//!
//! A language server covering domain-specific languages written for Doom's source ports.

mod langs;
mod lines;
mod names;
mod notif;
mod paths;
mod project;
mod request;
mod semtokens;
mod util;

use std::{
	cell::RefCell,
	collections::VecDeque,
	fmt::Debug,
	hash::BuildHasherDefault,
	ops::ControlFlow,
	path::{Path, PathBuf},
	rc::Rc,
};

use crossbeam_channel::SendError;
use indexmap::IndexSet;
use lsp_server::{
	Connection, ErrorCode, Message, Notification, ProtocolError, Request, RequestId, Response,
	ResponseError,
};
use lsp_types::{
	notification::{Notification as NotificationTrait, ShowMessage},
	request::{Request as RequestTrait, WorkspaceConfiguration},
	ConfigurationItem, ConfigurationParams, HoverProviderCapability, InitializeParams, MessageType,
	OneOf, SaveOptions, SemanticTokensFullOptions, SemanticTokensOptions,
	SemanticTokensServerCapabilities, ServerCapabilities, ShowMessageParams,
	TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
	TextDocumentSyncSaveOptions, WorkDoneProgressOptions,
};
use names::StringInterner;
use paths::FileId;
use project::Project;
use rustc_hash::{FxHashMap, FxHasher};
use serde::Serialize;
use tracing::{debug, error, info};
use tracing_subscriber::{
	fmt::writer::BoxMakeWriter, prelude::__tracing_subscriber_SubscriberExt,
	util::SubscriberInitExt,
};

use crate::project::{Scope, StackedScope};

pub(crate) use self::langs::*;

fn main() -> Result<(), ErrorBox> {
	log_init();
	info!("Initializing...");
	let (conn, threads) = lsp_server::Connection::stdio();
	let params = conn.initialize(serde_json::to_value(capabilities())?)?;

	let mut core = Core::new(params);

	core.comms.send(
		&conn,
		WorkspaceConfiguration::METHOD,
		ConfigurationParams {
			items: vec![ConfigurationItem {
				scope_uri: None,
				section: Some("doomls".to_string()),
			}],
		},
		Core::initial_config_received,
	)?;

	core.main_loop(conn)?;
	threads.join()?;
	info!("Shutdown complete.");
	Ok(())
}

fn log_init() {
	/// Like [`tracing_subscriber::fmt::time::Uptime`] but with
	/// hour/minute/second formatting for better clarity.
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	struct Uptime(std::time::Instant);

	impl Default for Uptime {
		fn default() -> Self {
			Self(std::time::Instant::now())
		}
	}

	impl tracing_subscriber::fmt::time::FormatTime for Uptime {
		fn format_time(
			&self,
			w: &mut tracing_subscriber::fmt::format::Writer<'_>,
		) -> std::fmt::Result {
			let elapsed = self.0.elapsed();
			let mins = elapsed.as_secs() / 60;
			let hours = mins / 60;
			let secs = elapsed.as_secs() % 60;
			write!(w, "{hours:02}:{mins:02}:{secs:02}")
		}
	}

	let timer = Uptime::default();
	eprintln!("Attempting log initialization.");
	let layer_stdout = tracing_subscriber::fmt::Layer::default()
		.with_timer(timer)
		.with_ansi(false)
		.with_writer(BoxMakeWriter::new(std::io::stderr));
	let collector = tracing_subscriber::registry().with(layer_stdout);
	collector.init();
}

#[must_use]
fn capabilities() -> ServerCapabilities {
	ServerCapabilities {
		text_document_sync: Some(TextDocumentSyncCapability::Options(
			TextDocumentSyncOptions {
				open_close: Some(true),
				change: Some(TextDocumentSyncKind::INCREMENTAL),
				will_save: None,
				will_save_wait_until: None,
				save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
					include_text: None,
				})),
			},
		)),
		definition_provider: Some(OneOf::Left(true)),
		document_symbol_provider: Some(OneOf::Left(true)),
		hover_provider: Some(HoverProviderCapability::Simple(true)),
		references_provider: Some(OneOf::Left(true)),
		semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
			SemanticTokensOptions {
				work_done_progress_options: WorkDoneProgressOptions::default(),
				legend: semtokens::legend(),
				range: Some(true),
				full: Some(SemanticTokensFullOptions::Bool(true)),
			},
		)),
		..Default::default()
	}
}

#[derive(Debug)]
pub(crate) struct Core {
	/// Is `false` when the application starts and only gets set to `true` when
	/// the client provides the `doomls.loadOrder` config for the first time.
	/// Until then, requests get back-logged in `comms` so they can be processed
	/// when the server has its full necessary context.
	pub(crate) init: bool,
	pub(crate) strings: StringInterner,
	pub(crate) projects: Vec<Project>,
	pub(crate) comms: CommSystem,
}

impl Core {
	#[must_use]
	fn new(params: serde_json::Value) -> Self {
		let _: InitializeParams = serde_json::from_value(params).unwrap();

		Self {
			init: false,
			strings: StringInterner::default(),
			projects: vec![],
			comms: CommSystem::default(),
		}
	}

	fn main_loop(&mut self, conn: Connection) -> UnitResult {
		for msg in conn.receiver.iter() {
			match msg {
				lsp_server::Message::Request(req) => {
					if conn.handle_shutdown(&req).map_err(Error::from)? {
						info!("Server shutting down...");
						return Ok(());
					}

					if self.projects.is_empty() {
						continue;
					}

					if self.init {
						while let Some(r) = self.comms.backlog_reqs.pop_front() {
							self.process_request(&conn, r);
						}

						self.process_request(&conn, req);
					} else {
						self.backlog_request(req);
					}
				}
				lsp_server::Message::Response(resp) => {
					let callback = self.comms.on_receive(&resp).unwrap();

					if let Err(err) = callback(self, &conn, resp) {
						error!("{err}");
					}
				}
				lsp_server::Message::Notification(notif) => {
					if self.init {
						while let Some(n) = self.comms.backlog_notifs.pop_front() {
							self.process_notif(&conn, n);
						}

						self.process_notif(&conn, notif);
					} else {
						self.backlog_notif(notif);
					}
				}
			}
		}

		Ok(())
	}

	fn process_request(&mut self, conn: &Connection, req: Request) {
		match request::handle(self, conn, req) {
			ControlFlow::Break(Err(error)) => {
				match error {
					Error::Channel(err) => {
						// Don't bother trying to send an error response if we
						// have reason to believe the channel is compromised.
						error!("{err}");
					}
					Error::Response(resp) => {
						{
							let e = resp.error.as_ref().unwrap();
							error!("{e:#?}");
						}

						let send_res = conn.sender.send(Message::Response(resp));

						if let Err(e) = send_res {
							error!("Failed to send error message: {e}");
						}
					}
					Error::Process { .. } => unreachable!(),
				}
			}
			ControlFlow::Continue(_) | ControlFlow::Break(_) => {}
		}
	}

	fn backlog_request(&mut self, req: Request) {
		if req.method.as_str() == lsp_types::request::SemanticTokensFullRequest::METHOD {
			self.comms.backlog_reqs.push_back(req);
		}
	}

	fn process_notif(&mut self, conn: &Connection, notif: Notification) {
		match notif::handle(self, conn, notif) {
			ControlFlow::Break(Err(error)) => {
				match error {
					Error::Channel(err) => {
						// Don't bother trying to send an error response if we
						// have reason to believe the channel is compromised.
						error!("{err}");
					}
					err @ Error::Process { .. } => {
						let send_res = conn.sender.send(Message::Notification(Notification {
							method: ShowMessage::METHOD.to_string(),
							params: serde_json::to_value(ShowMessageParams {
								typ: MessageType::ERROR,
								message: err.to_string(),
							})
							.unwrap(),
						}));

						if let Err(e) = send_res {
							error!("Failed to send error message: {e}");
						}
					}
					Error::Response(_) => unreachable!(),
				}
			}
			ControlFlow::Continue(_) | ControlFlow::Break(_) => {}
		}
	}

	fn backlog_notif(&mut self, notif: Notification) {
		if notif.method.as_str() == lsp_types::notification::DidOpenTextDocument::METHOD {
			self.comms.backlog_notifs.push_back(notif);
		}
	}

	fn semantic_update(&mut self) {
		let start_time = std::time::Instant::now();

		for project in &mut self.projects {
			project.update_global_symbols(&self.strings);
		}

		tracing::debug!(
			"Server-wide semantic update done in {}ms.",
			start_time.elapsed().as_millis()
		);
	}

	#[must_use]
	fn find_project_by_path(&self, path: &Path) -> Option<(&Project, FileId)> {
		self.projects
			.iter()
			.find_map(|project| project.get_fileid(path).map(|id| (project, id)))
	}

	#[must_use]
	fn find_project_by_path_mut(&mut self, path: &Path) -> Option<(&mut Project, FileId)> {
		self.projects
			.iter_mut()
			.find_map(|project| project.get_fileid(path).map(|id| (project, id)))
	}

	#[must_use]
	fn find_project_by_child(&self, path: &Path) -> Option<&Project> {
		self.projects
			.iter()
			.find(|project| util::path_is_child_of(path, project.root()))
	}

	#[must_use]
	fn find_project_by_child_mut(&mut self, path: &Path) -> Option<&mut Project> {
		self.projects
			.iter_mut()
			.find(|project| util::path_is_child_of(path, project.root()))
	}

	#[must_use]
	fn project_index(&self, project: &Project) -> usize {
		self.projects
			.iter()
			.position(|p| std::ptr::eq(p, project))
			.unwrap()
	}

	#[must_use]
	fn scope_stack(&self) -> Vec<StackedScope> {
		thread_local! {
			static NATIVE_SYMBOLS: RefCell<Rc<Scope>> = Default::default();
		}

		let mut ret = vec![];

		ret.push(StackedScope {
			ix_project: None,
			inner: NATIVE_SYMBOLS.with(|n| {
				let mut ptr = n.borrow_mut();

				if !ptr.is_empty() {
					return ptr.clone();
				}

				let scope = Rc::get_mut(&mut ptr).unwrap();

				for (iname, dat) in langs::zscript::sema::native_symbols(self) {
					scope.insert(iname, project::Datum::ZScript(dat));
				}

				ptr.clone()
			}),
			is_addendum: false,
		});

		for (i, project) in self.projects.iter().enumerate() {
			ret.push(StackedScope {
				ix_project: Some(i),
				inner: project.scope().clone(),
				is_addendum: false,
			});
		}

		ret
	}

	fn respond_null(conn: &Connection, id: RequestId) -> UnitResult {
		conn.sender
			.send(Message::Response(Response {
				id,
				result: Some(serde_json::Value::Null),
				error: None,
			}))
			.map_err(Error::from)
	}

	fn info_message(conn: &Connection, text: &'static str) -> UnitResult {
		conn.sender
			.send(Message::Notification(Notification {
				method: ShowMessage::METHOD.to_string(),
				params: serde_json::to_value(ShowMessageParams {
					typ: MessageType::INFO,
					message: text.to_string(),
				})
				.unwrap(),
			}))
			.map_err(Error::from)
	}

	// Setup ///////////////////////////////////////////////////////////////////

	fn build_project_list(&mut self, values: &[serde_json::Value]) {
		let load_order = values
			.iter()
			.filter_map(|lo_val| match lo_val.as_str() {
				Some(s) => Some(PathBuf::from(s)),
				None => {
					error!("Non-string load order item given: {lo_val:#?}");
					None
				}
			})
			.collect::<Vec<_>>();

		debug!("Rebuilding project list...");

		self.projects.clear();

		for path in load_order {
			debug!("Registering project: {}", path.display());
			let mut project = Project::new(path);

			// We have to intern all relevant paths here since (G)ZDoom `#include`
			// directives work case-insensitively on the ZDoom VFS. We have no
			// reason to believe that any given include path can just be joined
			// to the project root to get a valid OS path.

			let walker = walkdir::WalkDir::new(project.root())
				.follow_links(false)
				.max_depth(16)
				.same_file_system(true)
				.into_iter()
				.filter_map(|result| match result {
					Ok(d_e) => Some(d_e),
					Err(err) => {
						error!("Failed to inspect a project file: {err}");
						None
					}
				});

			for d_ent in walker {
				let path = d_ent.path();

				if path.is_dir() {
					continue;
				}

				// It's normal for Doom engine mods to come with all manner of
				// non-text resources like graphics and sounds.
				// There may also be compiled ACS blobs (`*.o`).
				// We have no reason to intern paths to these.

				const BIN_EXTS: &[&str] = &[
					"blend", "bmp", "dat", "flac", "iqm", "jpg", "jpeg", "lmp", "mid", "md3",
					"mp3", "ogg", "o", "pcx", "png", "tga", "wad", "wav", "xcf", "xm",
				];

				if path
					.extension()
					.is_some_and(|ext| BIN_EXTS.iter().any(|e| ext.eq_ignore_ascii_case(e)))
				{
					continue;
				}

				let _ = project.paths_mut().get_or_intern_nocase(path);
			}

			if project.root().is_dir() {
				project.build_include_trees();
			}

			self.projects.push(project);
		}
	}

	fn initial_config_received(&mut self, conn: &Connection, resp: Response) -> UnitResult {
		let Some(value) = resp.result else { return Ok(()); };

		let configs = match serde_json::from_value::<CfgReqResult>(value) {
			Ok(c) => c,
			Err(err) => {
				return Err(Error::Process {
					source: Some(Box::new(err)),
					ctx: "failed to decode user config".to_string(),
				});
			}
		};

		let string_array = configs.iter().find_map(|value| {
			let Some(obj) = value.as_object() else { return None; };
			let Some(v) = obj.get("loadOrder") else { return None; };
			v.as_array()
		});

		let Some(string_array) = string_array else {
			Core::info_message(conn, "No load order is configured; DoomLS functionality will be minimal.")?;
			return Ok(());
		};

		self.build_project_list(string_array);
		self.init = true;
		Ok(())
	}
}

#[derive(Debug, Default)]
struct CommSystem {
	next_id: i32,
	egress: FxHashMap<RequestId, ResponseCallback>,
	backlog_reqs: VecDeque<Request>,
	backlog_notifs: VecDeque<Notification>,
}

impl CommSystem {
	fn send<T: Serialize>(
		&mut self,
		conn: &Connection,
		method: &'static str,
		params: T,
		callback: ResponseCallback,
	) -> UnitResult {
		let id = RequestId::from(self.next_id);
		self.next_id = self.next_id.checked_add(1).unwrap_or(0);

		conn.sender
			.send(Message::Request(Request {
				id: id.clone(),
				method: method.to_string(),
				params: serde_json::to_value(params).unwrap(),
			}))
			.map_err(Error::from)?;

		let _ = self.egress.insert(id, callback);

		Ok(())
	}

	#[must_use]
	fn on_receive(&mut self, resp: &Response) -> Option<ResponseCallback> {
		self.egress.remove(&resp.id)
	}
}

// Types ///////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub(crate) enum Error {
	/// Failures to send messages over the LSP connection are given a separate
	/// variant to tell top-level code not to use the channel to report the error.
	Channel(SendError<Message>),
	Response(Response),
	Process {
		source: Option<ErrorBox>,
		ctx: String,
	},
}

impl Error {
	#[must_use]
	pub(crate) fn map_to_response(self, req_id: RequestId, code: ErrorCode) -> Self {
		if matches!(self, Self::Response(_)) {
			return self;
		}

		let message = self.to_string();

		Self::Response(Response {
			id: req_id,
			result: None,
			error: Some(ResponseError {
				code: code as i32,
				message,
				data: None,
			}),
		})
	}
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Channel(err) => write!(f, "failed to send a message: {err}"),
			Self::Process { source, ctx } => match source {
				Some(s) => {
					write!(f, "{ctx}: {s}")
				}
				None => {
					write!(f, "{ctx}")
				}
			},
			Self::Response(resp) => {
				write!(f, "failed to respond to a request: {resp:#?}")
			}
		}
	}
}

impl From<SendError<Message>> for Error {
	fn from(value: SendError<Message>) -> Self {
		Self::Channel(value)
	}
}

impl From<ProtocolError> for Error {
	fn from(value: ProtocolError) -> Self {
		Self::Process {
			source: Some(Box::new(value)),
			ctx: "Language Server Protocol error".to_string(),
		}
	}
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Self::Process {
			source: Some(Box::new(value)),
			ctx: "file I/O failure".to_string(),
		}
	}
}

pub(crate) type ErrorBox = Box<dyn std::error::Error + Send + Sync>;
pub(crate) type UnitResult = Result<(), Error>;
pub(crate) type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LangId {
	ZScript,
	Unknown,
}

type ResponseCallback = fn(&mut Core, &Connection, Response) -> UnitResult;
type CfgReqResult = <WorkspaceConfiguration as lsp_types::request::Request>::Result;
