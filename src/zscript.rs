//! Infrastructure related to the [ZScript](doomfront::zdoom::zscript) language.

mod highlight;

use doomfront::zdoom::zscript::{Syn, SyntaxNode};
use lsp_server::{Connection, Message, RequestId, Response};
use lsp_types::{
	Hover, HoverContents, HoverParams, LanguageString, MarkedString, SemanticTokens,
	SemanticTokensResult,
};
use tracing::warn;

use crate::{db::GreenFile, semtokens::Highlighter, Core, UnitResult};

impl Core {
	pub(super) fn zscript_req_hover(
		&self,
		conn: &Connection,
		gfile: GreenFile,
		id: RequestId,
		params: HoverParams,
	) -> UnitResult {
		let pos = params.text_document_position_params.position;

		let Some(token) = gfile.token_at::<Syn>(pos) else {
			warn!("Failed to find token specified by a hover request.");

			conn.sender.send(Message::Response(
				Response {
					id,
					result: None,
					error: None, // TODO
				}
			))?;

			return Ok(());
		};

		let contents = match token.kind() {
			Syn::KwClass => {
				HoverContents::Array(vec![
					MarkedString::LanguageString(LanguageString {
						language: "zscript".to_string(),
						value: "class".to_string(),
					}),
					MarkedString::String("A class defines an object type within ZScript, and is most of what you'll be creating within the language.".to_string())
					])
			}
			Syn::KwStruct => {
				HoverContents::Array(vec![
					MarkedString::LanguageString(LanguageString {
						language: "zscript".to_string(),
						value: "struct".to_string(),
					}),
					MarkedString::String("A structure is an object type that does not inherit from Object and is not always — though occasionally is — a reference type, unlike classes.".to_string())
					])
			}
			_ => {
				conn.sender.send(Message::Response(
					Response {
						id,
						result: Some(serde_json::Value::Null),
						error: None,
					}
				))?;

				return Ok(());
			}
		};

		let resp = Response {
			id,
			result: Some(serde_json::to_value(Hover {
				contents,
				range: None,
			})?),
			error: None,
		};

		conn.sender.send(Message::Response(resp))?;
		Ok(())
	}

	pub(super) fn zscript_req_semtokens_full(
		&self,
		conn: &Connection,
		gfile: GreenFile,
		id: RequestId,
	) -> UnitResult {
		let cursor = SyntaxNode::new_root(gfile.root);
		let mut highlighter = Highlighter::new(gfile.newlines);
		highlight::walk_tree(&mut highlighter, cursor);

		let resp = Response {
			id,
			result: Some(serde_json::to_value(SemanticTokensResult::Tokens(
				SemanticTokens {
					result_id: None,
					data: highlighter.tokens,
				},
			))?),
			error: None,
		};

		conn.sender.send(Message::Response(resp))?;
		Ok(())
	}
}
