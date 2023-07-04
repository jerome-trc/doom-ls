//! # DoomLS
//!
//! A language server covering domain-specific languages written for Doom's source ports.

// Common //////////////////////////////////////////////////////////////////////
mod db;
mod intern;
mod lines;
mod semtokens;
mod util;
// Languages ///////////////////////////////////////////////////////////////////
mod zscript;

use std::{ops::ControlFlow, sync::Arc};

use db::Source;
use intern::Interner;
use lines::LineIndex;
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
	notification::{DidChangeTextDocument, DidChangeWatchedFiles, DidOpenTextDocument},
	request::{
		GotoDefinition, HoverRequest, SemanticTokensFullRequest, SemanticTokensRangeRequest,
	},
	FileChangeType, GotoDefinitionResponse, HoverProviderCapability, InitializeParams, OneOf,
	SaveOptions, SemanticTokensFullOptions, SemanticTokensOptions,
	SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentSyncCapability,
	TextDocumentSyncKind, TextDocumentSyncOptions, TextDocumentSyncSaveOptions,
	WorkDoneProgressOptions,
};
use tracing::{error, info};
use tracing_subscriber::{
	fmt::{time::Uptime, writer::BoxMakeWriter},
	prelude::__tracing_subscriber_SubscriberExt,
	util::SubscriberInitExt,
};

use crate::db::{Compiler, DatabaseImpl};

pub(crate) type UnitResult = Result<(), Box<dyn std::error::Error + Send + Sync>>;

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
		req = Self::try_request::<SemanticTokensRangeRequest, _>(req, |id, params| {
			let path = util::uri_to_pathbuf(&params.text_document.uri)?;

			let Some(gfile_k) = self.db.try_file(path.into_boxed_path()) else {
				return Self::respond_null(conn, id);
			};

			let gfile = self.db.lookup_intern_file(gfile_k);

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

			let Some(gfile_k) = self.db.try_file(path.into_boxed_path()) else {
				return Self::respond_null(conn, id);
			};

			let gfile = self.db.lookup_intern_file(gfile_k);

			match gfile.lang {
				LangId::ZScript => {
					return self.zscript_req_hover(conn, gfile, id, params);
				}
				_ => Self::respond_null(conn, id),
			}
		})?;

		req = Self::try_request::<SemanticTokensFullRequest, _>(req, |id, params| {
			let path = util::uri_to_pathbuf(&params.text_document.uri)?;

			let Some(gfile_k) = self.db.try_file(path.into_boxed_path()) else {
				return Self::respond_null(conn, id);
			};

			let gfile = self.db.lookup_intern_file(gfile_k);

			match gfile.lang {
				LangId::ZScript => {
					return self.zscript_req_semtokens_full(conn, gfile, id);
				}
				_ => Self::respond_null(conn, id),
			}
		})?;

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

		ControlFlow::Continue(req)
	}

	fn handle_notif(&mut self, mut notif: Notification) -> ControlFlow<UnitResult, Notification> {
		notif = Self::try_notif::<DidChangeTextDocument, _>(notif, |params| {
			let path = util::uri_to_pathbuf(&params.text_document.uri)?.into_boxed_path();

			let lang = match self.db.try_file(path.clone()) {
				Some(k) => self.db.lookup_intern_file(k).lang,
				None => LangId::Unknown,
			};

			let mut new_text = self.db.source(path.clone()).text.to_string();

			lines::splice_changes(&mut new_text, params.content_changes);
			let lndx = Arc::new(LineIndex::new(&new_text));

			// TODO:
			// - Prevent needing to re-parse the entire file.
			// - Try reducing how many times the line index needs to be recomputed.

			self.db.set_source(
				path,
				Source {
					text: new_text.into(),
					lang,
					lndx,
				},
			);

			Ok(())
		})?;

		notif = Self::try_notif::<DidOpenTextDocument, _>(notif, |params| {
			let lang_id = match params.text_document.language_id.as_str() {
				"zscript" => LangId::ZScript,
				_ => return Ok(()),
			};

			let src_key = util::uri_to_pathbuf(&params.text_document.uri)?.into_boxed_path();
			let lndx = Arc::new(LineIndex::new(&params.text_document.text));

			self.db.set_source(
				src_key,
				Source {
					text: params.text_document.text.into(),
					lang: lang_id,
					lndx,
				},
			);

			Ok(())
		})?;

		notif = Self::try_notif::<DidChangeWatchedFiles, _>(notif, |params| {
			for change in params.changes {
				let path = util::uri_to_pathbuf(&change.uri)?.into_boxed_path();

				match change.typ {
					FileChangeType::CHANGED => {
						let Some(gfile_k) = self.db.try_file(path.clone()) else { continue; };
						let gfile = self.db.lookup_intern_file(gfile_k);
						let text = std::fs::read_to_string(&path)?;
						let lndx = Arc::new(LineIndex::new(&text));

						self.db.set_source(
							path,
							Source {
								text: text.into(),
								lang: gfile.lang,
								lndx,
							},
						);
					}
					FileChangeType::CREATED => {
						let text = std::fs::read_to_string(&path)?;
						let lndx = Arc::new(LineIndex::new(&text));

						self.db.set_source(
							path,
							Source {
								text: text.into(),
								lang: LangId::Unknown,
								lndx,
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

	fn respond_null(conn: &Connection, id: RequestId) -> UnitResult {
		conn.sender.send(Message::Response(Response {
			id,
			result: Some(serde_json::Value::Null),
			error: None,
		}))?;

		Ok(())
	}
}
