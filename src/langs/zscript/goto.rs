//! [Handler](req_goto) for the `textDocument/definition` request.

use std::ops::ControlFlow;

use doomfront::{
	rowan::ast::AstNode,
	zdoom::{
		ast::LitToken,
		zscript::{ast, Syn, SyntaxNode, SyntaxToken},
	},
};
use lsp_server::{ErrorCode, Message, Response};
use lsp_types::{GotoDefinitionResponse, Position};

use crate::{lines::LineCol, request, Core, Error, ErrorBox, UnitResult};

use super::location_by_inames;

pub(crate) fn req_goto(ctx: request::Context, position: Position) -> UnitResult {
	let parsed = ctx.sfile.parsed.as_ref().unwrap();
	let cursor = SyntaxNode::new_root(parsed.green.clone());

	let linecol = LineCol {
		line: position.line,
		col: position.character,
	};

	let Some(boffs) = ctx.sfile.lndx.offset(linecol) else {
		return Err(Error::Process {
			source: None,
			ctx: format!("failed to find token at position {linecol:#?}")
		}.map_to_response(ctx.id, ErrorCode::InvalidParams));
	};

	let Some(token) = cursor.token_at_offset(boffs).next() else {
		return Err(Error::Process {
			source: None,
			ctx: format!("failed to find token at position {linecol:#?}")
		}.map_to_response(ctx.id, ErrorCode::InvalidParams));
	};

	match token.kind() {
		Syn::Ident => {
			let parent = token.parent().unwrap();

			if token.text().eq_ignore_ascii_case("self") && ast::IdentExpr::can_cast(parent.kind())
			{
				// No useful information to provide here.
				Core::respond_null(ctx.conn, ctx.id)?;
				tracing::debug!("GotoDefinition miss - `self` identifier.");
				return Ok(());
			}

			match goto_ident(&ctx, token) {
				ControlFlow::Continue(()) => {
					tracing::debug!("GotoDefinition miss - unknown symbol.");
					Core::respond_null(ctx.conn, ctx.id)
				}
				ControlFlow::Break(Err(err)) => Err(Error::Process {
					source: Some(err),
					ctx: "go-to definition error".to_string(),
				}),
				ControlFlow::Break(Ok(resp)) => ctx
					.conn
					.sender
					.send(Message::Response(Response {
						id: ctx.id,
						result: Some(serde_json::to_value(resp).unwrap()),
						error: None,
					}))
					.map_err(Error::from),
			}
		}
		Syn::NameLit => match goto_name(&ctx, token) {
			ControlFlow::Continue(()) => {
				tracing::debug!("GotoDefinition miss - unknown symbol.");
				Core::respond_null(ctx.conn, ctx.id)
			}
			ControlFlow::Break(Err(err)) => Err(Error::Process {
				source: Some(err),
				ctx: "go-to definition error".to_string(),
			}),
			ControlFlow::Break(Ok(resp)) => ctx
				.conn
				.sender
				.send(Message::Response(Response {
					id: ctx.id,
					result: Some(serde_json::to_value(resp).unwrap()),
					error: None,
				}))
				.map_err(Error::from),
		},
		other => {
			// TODO:
			// - `Syn::StringLit`, which may be coerced to `name` and identify a class.
			// Also support LANGUAGE IDs, GLDEFS, maybe even CVar names.
			// - `Syn::KwSuper`; try to go to a parent class definition.
			// - `Syn::DocComment`; support following intra-doc links.
			// - `Syn::KwString` / `Syn::KwArray` / `Syn::KwMap` / `Syn::KwMapIterator` /
			// `Syn::KwColor` / `Syn::KwVector2` / `Syn::KwVector3` by faking their definitions.
			Core::respond_null(ctx.conn, ctx.id)?;
			tracing::debug!("GotoDefinition miss - unsupported token {other:#?}.");
			Ok(())
		}
	}

	// TODO: If the user put in a "go to definition" request on the identifier
	// making up the declaration, respond with a list of references.
}

fn goto_ident(
	ctx: &request::Context,
	token: SyntaxToken,
) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
	let iname_tgt_t = ctx.core.strings.type_name_nocase(token.text());
	let iname_tgt_v = ctx.core.strings.value_name_nocase(token.text());

	let Some(scopes) = super::prepare_scope_stack(ctx, &token) else {
		return ControlFlow::Continue(());
	};

	location_by_inames(ctx, &scopes, [iname_tgt_t, iname_tgt_v], &token)
}

fn goto_name(
	ctx: &request::Context,
	token: SyntaxToken,
) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
	let scopes = ctx.core.scope_stack();
	let lit = LitToken::new(token);
	let text = lit.name().unwrap();
	let iname = ctx.core.strings.type_name_nocase(text);
	location_by_inames(ctx, &scopes, [iname], lit.syntax())
}
