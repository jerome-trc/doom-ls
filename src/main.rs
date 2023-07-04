//! # DoomLS
//!
//! A language server covering domain-specific languages written for Doom's source ports.

// Common //////////////////////////////////////////////////////////////////////
mod db;
mod intern;
mod scan;
mod semtokens;
// Languages ///////////////////////////////////////////////////////////////////
mod zscript;

use std::{
	ops::ControlFlow,
	path::{Path, PathBuf},
};

use db::{GreenFile, LangId};
use doomfront::rowan::{GreenNode, SyntaxKind};
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
	notification::{DidChangeTextDocument, DidChangeWatchedFiles, DidOpenTextDocument},
	request::{GotoDefinition, HoverRequest, SemanticTokensFullRequest},
	FileChangeType, GotoDefinitionResponse, HoverProviderCapability, InitializeParams, OneOf,
	SaveOptions, SemanticTokensFullOptions, SemanticTokensOptions,
	SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentContentChangeEvent,
	TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
	TextDocumentSyncSaveOptions, Url, WorkDoneProgressOptions,
};
use tracing::{error, info};
use tracing_subscriber::{
	fmt::{time::Uptime, writer::BoxMakeWriter},
	prelude::__tracing_subscriber_SubscriberExt,
	util::SubscriberInitExt,
};

use crate::db::{Compiler, DatabaseImpl};

pub(crate) type UnitResult = Result<(), Box<dyn std::error::Error + Send + Sync>>;

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
				range: Some(false), // TODO: Support this.
				full: Some(SemanticTokensFullOptions::Bool(true)),
			},
		)),
		..Default::default()
	}
}

pub(crate) struct Core {
	pub(crate) db: DatabaseImpl,
}

impl Core {
	#[must_use]
	fn new(params: serde_json::Value) -> Self {
		let _: InitializeParams = serde_json::from_value(params).unwrap();

		Self {
			db: DatabaseImpl::default(),
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

					match self.handle_request(&conn, req) {
						ControlFlow::Break(Err(err)) => {
							error!("{err}");
						}
						ControlFlow::Continue(_) | ControlFlow::Break(_) => {}
					}
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

	fn handle_request(
		&self,
		conn: &Connection,
		mut req: Request,
	) -> ControlFlow<UnitResult, Request> {
		req = Self::try_request::<GotoDefinition, _>(req, |id, _params| {
			let result = Some(GotoDefinitionResponse::Array(vec![]));
			let result = serde_json::to_value(result)?;

			let resp = Response {
				id,
				result: Some(result),
				error: None,
			};

			conn.sender.send(Message::Response(resp))?;
			Ok(())
		})?;

		req = Self::try_request::<HoverRequest, _>(req, |id, params| {
			let path =
				Self::uri_to_pathbuf(&params.text_document_position_params.text_document.uri)?;
			let gfile = self.db.file(path.into_boxed_path());

			match gfile.lang {
				LangId::ZScript => {
					return self.zscript_req_hover(conn, gfile, id, params);
				}
				_ => {
					conn.sender.send(Message::Response(Response {
						id,
						result: Some(serde_json::Value::Null),
						error: None,
					}))?;

					Ok(())
				}
			}
		})?;

		req = Self::try_request::<SemanticTokensFullRequest, _>(req, |id, params| {
			let path = Self::uri_to_pathbuf(&params.text_document.uri)?;
			let gfile = self.db.file(path.into_boxed_path());

			match gfile.lang {
				LangId::ZScript => {
					return self.zscript_req_semtokens_full(conn, gfile, id);
				}
				_ => {
					conn.sender.send(Message::Response(Response {
						id,
						result: Some(serde_json::Value::Null),
						error: None,
					}))?;

					Ok(())
				}
			}
		})?;

		ControlFlow::Continue(req)
	}

	fn handle_notif(&mut self, mut notif: Notification) -> ControlFlow<UnitResult, Notification> {
		notif = Self::try_notif::<DidChangeTextDocument, _>(notif, |mut params| {
			let path = Self::uri_to_pathbuf(&params.text_document.uri)?.into_boxed_path();
			let Some(mut gfile) = self.db.try_file(path.clone()) else { return Ok(()); };

			let (parser, lex_ctx) = match gfile.lang {
				LangId::ZScript => (
					doomfront::zdoom::zscript::parse::file,
					doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
				),
				_ => return Ok(()),
			};

			// TODO: Only re-parse as much as necessary.
			let source = match params.content_changes.pop() {
				Some(TextDocumentContentChangeEvent {
					range: None,
					range_length: None,
					text,
				}) => text,
				_ => std::fs::read_to_string(path)?,
			};

			gfile.root = doomfront::parse(
				&source, parser,
				// TODO: Per-folder user config option for ZScript version.
				// Everything else should use 1.0.0.
				lex_ctx,
			)
			.into_inner();

			gfile.newlines = scan::compute_newlines(&source).into();

			Ok(())
		})?;

		notif = Self::try_notif::<DidOpenTextDocument, _>(notif, |params| {
			let (lang_id, parser, lex_ctx) = match params.text_document.language_id.as_str() {
				"zscript" => (
					LangId::ZScript,
					doomfront::zdoom::zscript::parse::file,
					doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
				),
				_ => return Ok(()),
			};

			let gfk = Self::uri_to_pathbuf(&params.text_document.uri)?.into_boxed_path();

			self.db.set_file(
				gfk,
				GreenFile {
					lang: lang_id,
					root: doomfront::parse(
						&params.text_document.text,
						parser,
						// TODO: Per-folder user config option for ZScript version.
						// Everything else should use 1.0.0.
						lex_ctx,
					)
					.into_inner(),
					newlines: scan::compute_newlines(&params.text_document.text).into(),
				},
			);

			Ok(())
		})?;

		notif = Self::try_notif::<DidChangeWatchedFiles, _>(notif, |params| {
			for change in params.changes {
				let path = Self::uri_to_pathbuf(&change.uri)?;

				match change.typ {
					FileChangeType::CHANGED | FileChangeType::CREATED => {
						self.db.set_file(
							path.into_boxed_path(),
							GreenFile {
								lang: LangId::Unknown,
								root: GreenNode::new(SyntaxKind(u16::MAX), []),
								newlines: vec![].into(),
							},
						);
					}
					FileChangeType::DELETED => {
						// TODO: Investigate if anything should be attempted here.
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

	fn uri_to_pathbuf(uri: &lsp_types::Url) -> Result<PathBuf, PathError> {
		if uri.scheme() != "file" {
			return Err(PathError::NonFileUri(uri.scheme().to_owned()));
		}

		let ret = match uri.to_file_path() {
			Ok(pb) => pb,
			Err(()) => return Err(PathError::NoHost),
		};

		ret.canonicalize().map_err(PathError::Canonicalize)
	}

	#[allow(unused)]
	fn path_to_uri(path: impl AsRef<Path>) -> Result<Url, PathError> {
		Url::parse(&format!("file://{}", path.as_ref().display()))
			.map_err(|err| PathError::UriParse(Box::new(err)))
	}
}

#[derive(Debug)]
enum PathError {
	Canonicalize(std::io::Error),
	NoHost,
	NonFileUri(String),
	UriParse(Box<dyn std::error::Error + Send + Sync>),
}

impl std::error::Error for PathError {}

impl std::fmt::Display for PathError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Canonicalize(err) => write!(f, "failed to canonicalize a path: {err}"),
			Self::NoHost => write!(f, "URI host is neither empty nor `localhost`"),
			Self::NonFileUri(scheme) => write!(f, "expected scheme `file`, found: {scheme}"),
			Self::UriParse(err) => write!(f, "failed to parse a `file` URI: {err}"),
		}
	}
}
