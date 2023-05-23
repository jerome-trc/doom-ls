//! # DoomLSP
//!
//! A language server covering domain-specific languages written for Doom's source ports.

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{request::GotoDefinition, GotoDefinitionResponse, InitializeParams};

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
	eprintln!("(DoomLSP) Initializing...");
	let (conn, threads) = lsp_server::Connection::stdio();
	let params = conn.initialize(serde_json::to_value(capabilities())?)?;
	main_loop(conn, params)?;
	threads.join()?;
	eprintln!("(DoomLSP) Shutdown complete.");
	Ok(())
}

fn main_loop(
	conn: Connection,
	params: serde_json::Value,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
	let _: InitializeParams = serde_json::from_value(params).unwrap();
	let _ = Core::default();

	for msg in &conn.receiver {
		match msg {
			lsp_server::Message::Request(req) => {
				if conn.handle_shutdown(&req)? {
					return Ok(());
				}

				match cast::<GotoDefinition>(req) {
					Ok((id, gdp)) => {
						eprintln!("(DoomLSP) Got GotoDefinition request (#{id}): {gdp:?}");
						let result = Some(GotoDefinitionResponse::Array(vec![]));
						let result = serde_json::to_value(&result).unwrap();
						let resp = Response {
							id,
							result: Some(result),
							error: None,
						};
						conn.sender.send(Message::Response(resp))?;
						continue;
					}
					Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
					Err(ExtractError::MethodMismatch(req)) => req,
				};
			}
			lsp_server::Message::Response(_) => {}
			lsp_server::Message::Notification(_) => {}
		}
	}

	Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
	R: lsp_types::request::Request,
	R::Params: serde::de::DeserializeOwned,
{
	req.extract(R::METHOD)
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

#[derive(Debug, Default)]
pub(crate) struct Core; // ???
