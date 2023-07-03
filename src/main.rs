//! # DoomLS
//!
//! A language server covering domain-specific languages written for Doom's source ports.

use std::{cell::RefCell, collections::hash_map, ops::ControlFlow, path::PathBuf};

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
	notification::{DidChangeTextDocument, DidChangeWatchedFiles, DidOpenTextDocument},
	request::GotoDefinition,
	FileChangeType, GotoDefinitionResponse, InitializeParams,
};
use rustc_hash::FxHashMap;
use tracing::{error, info, warn};
use tracing_subscriber::{
	fmt::{time::Uptime, writer::BoxMakeWriter},
	prelude::__tracing_subscriber_SubscriberExt,
	util::SubscriberInitExt,
};
type UnitResult = Result<(), Box<dyn std::error::Error + Send + Sync>>;

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
	let mut core = Core::new(conn, params);
	core.main_loop()?;
	threads.join()?;
	info!("Shutdown complete.");
	Ok(())
}

#[must_use]
fn capabilities() -> lsp_types::ServerCapabilities {
	lsp_types::ServerCapabilities {
		text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
			lsp_types::TextDocumentSyncOptions {
				open_close: Some(true),
				change: Some(lsp_types::TextDocumentSyncKind::INCREMENTAL),
				will_save: None,
				will_save_wait_until: None,
				save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(
					lsp_types::SaveOptions { include_text: None },
				)),
			},
		)),
		..Default::default()
	}
}

pub(crate) struct Core {
	pub(crate) conn: Connection,
	pub(crate) fs: RefCell<FxHashMap<PathBuf, Document>>,
}

#[derive(Debug)]
pub(crate) struct Document {
	_lang: LangId,
	source: String,
}

impl Core {
	#[must_use]
	fn new(conn: Connection, params: serde_json::Value) -> Self {
		let _: InitializeParams = serde_json::from_value(params).unwrap();

		Self {
			conn,
			fs: RefCell::new(FxHashMap::default()),
		}
	}

	fn main_loop(&mut self) -> UnitResult {
		for msg in &self.conn.receiver {
			match msg {
				lsp_server::Message::Request(req) => {
					if self.conn.handle_shutdown(&req)? {
						info!("Server shutting down...");
						return Ok(());
					}

					self.handle_request(req);
				}
				lsp_server::Message::Response(_) => {}
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

	fn handle_request(&self, mut req: Request) -> ControlFlow<UnitResult, Request> {
		req = Self::try_request::<GotoDefinition, _>(req, |id, _params| {
			let result = Some(GotoDefinitionResponse::Array(vec![]));
			let result = serde_json::to_value(result).unwrap();

			let resp = Response {
				id,
				result: Some(result),
				error: None,
			};

			self.conn.sender.send(Message::Response(resp))?;
			Ok(())
		})?;

		ControlFlow::Continue(req)
	}

	fn handle_notif(&self, mut notif: Notification) -> ControlFlow<UnitResult, Notification> {
		notif = Self::try_notif::<DidChangeTextDocument, _>(notif, |params| {
			// TODO: Only re-parse as much as necessary.
			let uri = params.text_document.uri;
			let path = Self::uri_to_pathbuf(uri)?;
			let mut fs = self.fs.borrow_mut();

			let hash_map::Entry::Occupied(mut occ) = fs.entry(path.clone()) else {
				warn!("Change made to file {} without it having been opened.", path.display());
				return Ok(());
			};

			let source = std::fs::read_to_string(path)?;
			occ.get_mut().source = source;
			Ok(())
		})?;

		notif = Self::try_notif::<DidOpenTextDocument, _>(notif, |params| {
			let lang_id = match params.text_document.language_id.as_str() {
				"zscript" => LangId::ZScript,
				_ => return Ok(()),
			};

			self.fs.borrow_mut().insert(
				Self::uri_to_pathbuf(params.text_document.uri)?,
				Document {
					_lang: lang_id,
					source: params.text_document.text,
				},
			);

			Ok(())
		})?;

		notif = Self::try_notif::<DidChangeWatchedFiles, _>(notif, |params| {
			for change in params.changes {
				let path = Self::uri_to_pathbuf(change.uri)?;

				match change.typ {
					FileChangeType::CHANGED | FileChangeType::CREATED => {
						todo!()
					}
					FileChangeType::DELETED => {
						if self.fs.borrow_mut().remove(&path).is_none() {
							warn!("Attempted removal of non-existent file: {}", path.display())
						}
					}
					_ => unreachable!(),
				}
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

	fn uri_to_pathbuf(uri: lsp_types::Url) -> Result<PathBuf, PathError> {
		if uri.scheme() != "file" {
			return Err(PathError::NonFileUri(uri.scheme().to_owned()));
		}

		let ret = match uri.to_file_path() {
			Ok(pb) => pb,
			Err(()) => return Err(PathError::NoHost),
		};

		ret.canonicalize().map_err(PathError::Canonicalize)
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FileKey {
	path: PathBuf,
	lang: LangId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum LangId {
	ZScript,
}

#[derive(Debug)]
enum PathError {
	NoHost,
	NonFileUri(String),
	Canonicalize(std::io::Error),
}

impl std::error::Error for PathError {}

impl std::fmt::Display for PathError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			PathError::NoHost => write!(f, "URI host is neither empty nor `localhost`"),
			PathError::NonFileUri(scheme) => write!(f, "expected scheme `file`, found: {scheme}"),
			PathError::Canonicalize(err) => write!(f, "failed to canonicalize a path: {err}"),
		}
	}
}
