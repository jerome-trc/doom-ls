//! # DoomLS
//!
//! A language server covering domain-specific languages written for Doom's source ports.

// Common //////////////////////////////////////////////////////////////////////
mod lines;
mod project;
mod semtokens;
mod util;
// Languages ///////////////////////////////////////////////////////////////////
mod zscript;

use std::{
	hash::BuildHasherDefault,
	ops::ControlFlow,
	path::{Path, PathBuf},
	sync::Arc,
};

use indexmap::IndexSet;
use lines::LineIndex;
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
	notification::{
		DidChangeConfiguration, DidChangeTextDocument, DidChangeWatchedFiles, DidSaveTextDocument,
		ShowMessage,
	},
	request::{
		GotoDefinition, HoverRequest, SemanticTokensFullRequest, SemanticTokensRangeRequest,
		WorkspaceConfiguration,
	},
	ConfigurationItem, ConfigurationParams, FileChangeType, HoverProviderCapability,
	InitializeParams, MessageType, OneOf, SaveOptions, SemanticTokensFullOptions,
	SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities, ShowMessageParams,
	TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
	TextDocumentSyncSaveOptions, WorkDoneProgressOptions,
};
use project::{Compiler, FileId, Project, Source};
use rustc_hash::{FxHashMap, FxHasher};
use serde::Serialize;
use tracing::{debug, error, info};
use tracing_subscriber::{
	fmt::{time::Uptime, writer::BoxMakeWriter},
	prelude::__tracing_subscriber_SubscriberExt,
	util::SubscriberInitExt,
};

#[derive(Debug)]
pub(crate) struct MsgError(String);

impl std::error::Error for MsgError {}

impl std::fmt::Display for MsgError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}

pub(crate) type ErrorBox = Box<dyn std::error::Error + Send + Sync>;
pub(crate) type UnitResult = Result<(), ErrorBox>;
pub(crate) type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LangId {
	ZScript,
	Unknown,
}

fn main() -> UnitResult {
	let timer = Uptime::default();
	eprintln!("Attempting log initialization.");
	let layer_stdout = tracing_subscriber::fmt::Layer::default()
		.with_timer(timer)
		.with_ansi(false)
		.with_writer(BoxMakeWriter::new(std::io::stderr));
	let collector = tracing_subscriber::registry().with(layer_stdout);
	collector.init();

	info!("Initializing...");
	let (conn, threads) = lsp_server::Connection::stdio();
	let params = conn.initialize(serde_json::to_value(capabilities())?)?;

	let mut core = Core::new(params);

	core.comms.send(
		&conn,
		<WorkspaceConfiguration as lsp_types::request::Request>::METHOD,
		ConfigurationParams {
			items: vec![ConfigurationItem {
				scope_uri: None,
				section: Some("doomls".to_string()),
			}],
		},
		|core, conn, resp| {
			let Some(value) = resp.result else { return Ok(()); };

			let configs = match serde_json::from_value::<CfgReqResult>(value) {
				Ok(c) => c,
				Err(err) => {
					error!("Failed to decode user config: {err}");
					return Err(Box::new(err));
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

			core.build_project_list(string_array);
			Ok(())
		},
	)?;

	core.main_loop(conn)?;
	threads.join()?;
	info!("Shutdown complete.");
	Ok(())
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
		hover_provider: Some(HoverProviderCapability::Simple(true)),
		semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
			SemanticTokensOptions {
				work_done_progress_options: WorkDoneProgressOptions::default(),
				legend: semtokens::legend(),
				range: Some(true), // TODO: Support this.
				full: Some(SemanticTokensFullOptions::Bool(true)),
			},
		)),
		..Default::default()
	}
}

pub(crate) struct Core {
	pub(crate) projects: Vec<Project>,
	pub(crate) comms: CommSystem,
}

impl Core {
	#[must_use]
	fn new(params: serde_json::Value) -> Self {
		let _: InitializeParams = serde_json::from_value(params).unwrap();

		Self {
			projects: vec![],
			comms: CommSystem::default(),
		}
	}

	fn main_loop(&mut self, conn: Connection) -> UnitResult {
		for msg in conn.receiver.iter() {
			match msg {
				lsp_server::Message::Request(req) => {
					if conn.handle_shutdown(&req)? {
						info!("Server shutting down...");
						return Ok(());
					}

					if self.projects.is_empty() {
						continue;
					}

					match self.handle_request(&conn, req) {
						ControlFlow::Break(Err(err)) => {
							error!("{err}");
						}
						ControlFlow::Continue(_) | ControlFlow::Break(_) => {}
					}
				}
				lsp_server::Message::Response(resp) => {
					let callback = self.comms.on_receive(&resp).unwrap();

					if let Err(err) = callback(self, &conn, resp) {
						error!("{err}");
					}
				}
				lsp_server::Message::Notification(notif) => match self.handle_notif(notif) {
					ControlFlow::Break(Err(err)) => {
						error!("{err}");
					}
					ControlFlow::Continue(_) | ControlFlow::Break(_) => {}
				},
			}
		}

		Ok(())
	}

	fn handle_request(
		&mut self,
		conn: &Connection,
		mut req: Request,
	) -> ControlFlow<UnitResult, Request> {
		req = Self::try_request::<SemanticTokensRangeRequest, _>(req, |id, params| {
			let path = util::uri_to_pathbuf(&params.text_document.uri)?;

			let Some((project, file_id)) = self.find_project_by_path(&path) else {
				return Self::respond_null(conn, id);
			};

			let Some(gfile_k) = project.try_get_gfile(file_id) else {
				return Self::respond_null(conn, id);
			};

			let gfile = project.lookup_gfile(gfile_k);

			match gfile.lang {
				LangId::ZScript => {
					return self.zscript_req_semtokens_full(conn, gfile, id);
				}
				_ => Self::respond_null(conn, id),
			}
		})?;

		req = Self::try_request::<HoverRequest, _>(req, |id, params| {
			let path =
				util::uri_to_pathbuf(&params.text_document_position_params.text_document.uri)?;

			let Some((project, file_id)) = self.find_project_by_path(&path) else {
				return Self::respond_null(conn, id);
			};

			let Some(gfile_k) = project.try_get_gfile(file_id) else {
				return Self::respond_null(conn, id);
			};

			let gfile = project.lookup_gfile(gfile_k);

			match gfile.lang {
				LangId::ZScript => {
					return self.zscript_req_hover(conn, gfile, id, params);
				}
				_ => Self::respond_null(conn, id),
			}
		})?;

		req = Self::try_request::<SemanticTokensFullRequest, _>(req, |id, params| {
			let path = util::uri_to_pathbuf(&params.text_document.uri)?;

			let Some((project, file_id)) = self.find_project_by_path(&path) else {
				return Self::respond_null(conn, id);
			};

			let Some(gfile_k) = project.try_get_gfile(file_id) else {
				return Self::respond_null(conn, id);
			};

			let gfile = project.lookup_gfile(gfile_k);

			match gfile.lang {
				LangId::ZScript => {
					return self.zscript_req_semtokens_full(conn, gfile, id);
				}
				_ => Self::respond_null(conn, id),
			}
		})?;

		req = Self::try_request::<GotoDefinition, _>(req, |id, params| {
			let path =
				util::uri_to_pathbuf(&params.text_document_position_params.text_document.uri)?;

			let Some((ix_p, gfile)) = self.projects.iter().enumerate().find_map(|(i, project)| {
				let file_id = match project.get_fileid(&path) {
					Some(id) => id,
					None => {
						return None;
					},
				};

				project.try_get_gfile(file_id).map(|gf_k| {
					(i, project.lookup_gfile(gf_k))
				})
			}) else {
				return Self::respond_null(conn, id);
			};

			let LangId::ZScript = gfile.lang else { return Self::respond_null(conn, id); };

			self.zscript_req_goto(
				conn,
				gfile,
				id,
				params.text_document_position_params.position,
				ix_p,
			)
		})?;

		ControlFlow::Continue(req)
	}

	fn handle_notif(&mut self, mut notif: Notification) -> ControlFlow<UnitResult, Notification> {
		notif = Self::try_notif::<DidChangeTextDocument, _>(notif, |params| {
			let path = util::uri_to_pathbuf(&params.text_document.uri)?;

			let Some((project, file_id)) = self.find_project_by_path_mut(&path) else {
				// The user is editing a file outside the load order.
				// Nothing to do here.
				return Ok(());
			};

			let lang = match project.try_get_gfile(file_id) {
				Some(k) => project.lookup_gfile(k).lang,
				None => LangId::Unknown,
			};

			let mut new_text = project.source(file_id).text.to_string();

			lines::splice_changes(&mut new_text, params.content_changes);
			let lndx = Arc::new(LineIndex::new(&new_text));

			// TODO:
			// - Prevent needing to re-parse the entire file.
			// - Try reducing how many times the line index needs to be recomputed.

			project.set_source(
				file_id,
				Source {
					text: new_text.into(),
					lang,
					lndx,
				},
			);

			Ok(())
		})?;

		notif = Self::try_notif::<DidChangeWatchedFiles, _>(notif, |params| {
			for change in params.changes {
				let path = util::uri_to_pathbuf(&change.uri)?;

				match change.typ {
					FileChangeType::CHANGED => {
						let Some((project, file_id)) = self.find_project_by_path_mut(&path) else {
							continue;
						};

						let Some(gfile_k) = project.try_get_gfile(file_id) else {
							continue;
						};

						let gfile = project.lookup_gfile(gfile_k);
						let text = std::fs::read_to_string(&path)?;
						let lndx = Arc::new(LineIndex::new(&text));

						project.set_source(
							file_id,
							Source {
								text: text.into(),
								lang: gfile.lang,
								lndx,
							},
						);
					}
					FileChangeType::CREATED => {
						let Some(project) = self.find_project_by_child_mut(&path) else {
							continue;
						};

						let text = std::fs::read_to_string(&path)?;
						let lndx = Arc::new(LineIndex::new(&text));
						let file_id = project.intern_pathbuf(path);

						project.set_source(
							file_id,
							Source {
								text: text.into(),
								lang: LangId::Unknown,
								lndx,
							},
						);
					}
					FileChangeType::DELETED => {
						let Some(project) = self.find_project_by_child_mut(&path) else {
							continue;
						};

						project.on_file_delete(path);
					}
					_ => unreachable!(),
				}
			}

			Ok(())
		})?;

		notif = Self::try_notif::<DidChangeConfiguration, _>(notif, |params| {
			debug!("Config changed: {:#?}", params.settings);

			let Some(obj) = params.settings.as_object() else { return Ok(()); };
			let Some(v) = obj.get("loadOrder") else { return Ok(()); };
			let Some(string_array) = v.as_array() else { return Ok(()); };
			self.build_project_list(string_array);
			Ok(())
		})?;

		notif = Self::try_notif::<DidSaveTextDocument, _>(notif, |params| {
			let path = util::uri_to_pathbuf(&params.text_document.uri)?;

			if path
				.file_stem()
				.is_some_and(|fstem| fstem.eq_ignore_ascii_case("ZSCRIPT"))
			{
				let Some(project) = self.find_project_by_child_mut(&path) else {
					return Ok(());
				};

				let _ = project.rebuild_zscript_include_tree(path);
				// TODO: Handle error, emit diagnostics.
			}

			Ok(())
		})?;

		ControlFlow::Continue(notif)
	}

	#[must_use]
	fn try_request<R, F>(req: Request, callback: F) -> ControlFlow<UnitResult, Request>
	where
		R: lsp_types::request::Request,
		F: FnOnce(RequestId, R::Params) -> UnitResult,
	{
		match req.extract::<R::Params>(R::METHOD) {
			Ok((reqid, params)) => ControlFlow::Break(callback(reqid, params)),
			Err(err) => Self::extract_error(err),
		}
	}

	#[must_use]
	fn try_notif<N, F>(notif: Notification, callback: F) -> ControlFlow<UnitResult, Notification>
	where
		N: lsp_types::notification::Notification,
		F: FnOnce(N::Params) -> UnitResult,
	{
		match notif.extract::<N::Params>(N::METHOD) {
			Ok(params) => ControlFlow::Break(callback(params)),
			Err(err) => Self::extract_error(err),
		}
	}

	#[must_use]
	fn extract_error<T>(err: ExtractError<T>) -> ControlFlow<UnitResult, T> {
		match err {
			ExtractError::MethodMismatch(t) => ControlFlow::Continue(t),
			ExtractError::JsonError { method: _, error } => {
				ControlFlow::Break(Err(Box::new(error)))
			}
		}
	}

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

				project.intern_path(path);
			}

			if project.root().is_dir() {
				project.build_include_trees();
			}

			self.projects.push(project);
		}
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
	fn find_project_by_child_mut(&mut self, path: &Path) -> Option<&mut Project> {
		self.projects
			.iter_mut()
			.find(|project| util::path_is_child_of(path, project.root()))
	}

	fn respond_null(conn: &Connection, id: RequestId) -> UnitResult {
		conn.sender.send(Message::Response(Response {
			id,
			result: Some(serde_json::Value::Null),
			error: None,
		}))?;

		Ok(())
	}

	fn info_message(conn: &Connection, text: &'static str) -> UnitResult {
		conn.sender.send(Message::Notification(Notification {
			method: <ShowMessage as lsp_types::notification::Notification>::METHOD.to_string(),
			params: serde_json::to_value(ShowMessageParams {
				typ: MessageType::INFO,
				message: text.to_string(),
			})
			.unwrap(),
		}))?;

		Ok(())
	}
}

#[derive(Debug, Default)]
struct CommSystem {
	next_id: i32,
	egress: FxHashMap<RequestId, ResponseCallback>,
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

		conn.sender.send(Message::Request(Request {
			id: id.clone(),
			method: method.to_string(),
			params: serde_json::to_value(params)?,
		}))?;

		let _ = self.egress.insert(id, callback);

		Ok(())
	}

	#[must_use]
	fn on_receive(&mut self, resp: &Response) -> Option<ResponseCallback> {
		self.egress.remove(&resp.id)
	}
}

type ResponseCallback = fn(&mut Core, &Connection, Response) -> UnitResult;
type CfgReqResult = <WorkspaceConfiguration as lsp_types::request::Request>::Result;
