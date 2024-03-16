//! Handling for `textDocument/definition` requests.

use doomfront::{
	rowan::{ast::AstNode, TextSize},
	zdoom::zscript::{ast, Syntax, SyntaxNode, SyntaxToken},
};
use lsp_server::{ErrorCode, Message, Response};
use lsp_types::{GotoDefinitionResponse, Location, Position, Url};
use tracing::debug;

use crate::{error::Error, request, util, UnitResult};

pub(crate) fn handle(ctx: request::Context, pos: TextSize) -> UnitResult {
	let cursor = SyntaxNode::new_root(ctx.src.green.as_ref().unwrap().clone());

	let Some(token) = cursor.token_at_offset(pos).next() else {
		return Err(Error::Process {
			source: None,
			ctx: format!("failed to find token at offset {pos:?}"),
		}
		.map_to_response(ctx.id, ErrorCode::InvalidParams));
	};

	if token.kind() != Syntax::StringLit {
		#[cfg(debug_assertions)]
		debug!("Goto-def miss - unsupported token.");
		// TODO:
		// - identifiers
		// - `super`, `self`, `invoker`
		// - zscdoc links
		// - virtual documents showing declarations for `string`, `array`, etc.?
		return util::respond_null(ctx.conn, ctx.id);
	}

	if let Some(include) = token.parent().and_then(ast::IncludeDirective::cast) {
		return include_directive_path(ctx, token, include);
	}

	#[cfg(debug_assertions)]
	debug!("Goto-def miss - string literal refers to nothing at this offset.");

	// TODO:
	// - LANGUAGE IDs
	// - GLDEFS
	// - CVar names

	util::respond_null(ctx.conn, ctx.id)
}

fn include_directive_path(
	ctx: request::Context,
	_: SyntaxToken,
	directive: ast::IncludeDirective,
) -> UnitResult {
	if ctx.project.zscript.root.is_none() {
		#[cfg(debug_assertions)]
		debug!("Goto-def miss - no ZScript root.");

		return util::respond_null(ctx.conn, ctx.id);
	}

	let Some(uncanon) = directive.include_path(&ctx.project.root, || {
		ctx.core.paths.resolve(ctx.src.id).parent().unwrap()
	}) else {
		#[cfg(debug_assertions)]
		debug!("Goto-def miss - parsing error");

		return util::respond_null(ctx.conn, ctx.id);
	};

	let full_path = match uncanon.canonicalize() {
		Ok(p) => p,
		Err(_) => {
			#[cfg(debug_assertions)]
			debug!("Goto-def miss - canonicalization");

			return util::respond_null(ctx.conn, ctx.id);
		}
	};

	let id = ctx.core.paths.intern(&full_path);

	if ctx.project.zscript.includes.contains_node(id) {
		let resp = GotoDefinitionResponse::Scalar(Location {
			uri: Url::from_file_path(full_path).unwrap(),
			range: lsp_types::Range {
				start: Position {
					line: 0,
					character: 0,
				},
				end: Position {
					line: 0,
					character: 0,
				},
			},
		});

		return ctx
			.conn
			.sender
			.send(Message::Response(Response {
				id: ctx.id,
				result: Some(serde_json::to_value(resp).unwrap()),
				error: None,
			}))
			.map_err(Error::from);
	}

	#[cfg(debug_assertions)]
	debug!(
		"Goto-def miss - include path points to non-existent/non-included file: {}",
		full_path.display()
	);

	util::respond_null(ctx.conn, ctx.id)
}
