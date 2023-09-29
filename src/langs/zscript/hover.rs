//! Handling for `textDocument/hover` requests.

mod keyword;

use doomfront::zdoom::zscript::{Syn, SyntaxToken};
use lsp_server::{Message, Response};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString, OneOf};
use tracing::debug;

use crate::{
	core::{FileSpan, SymGraphKey, SymGraphValue},
	request, util, UnitResult,
};

pub(crate) fn handle(ctx: request::Context, params: HoverParams) -> UnitResult {
	let pos = params.text_document_position_params.position;

	let Some(token_untyped) = ctx.src.token_at(pos) else {
		#[cfg(debug_assertions)]
		debug!("Hover info miss: no token information at hover position");

		return util::respond_null(ctx.conn, ctx.id);
	};

	let token = SyntaxToken::from(token_untyped);
	let span = token.text_range();

	let contents = if token.kind().is_keyword() {
		keyword::hover_info_for(token)
	} else if token.kind() == Syn::Ident {
		ident(&ctx, token)
	} else {
		// TODO:
		// - zsdoc links
		// - string literals
		// - name literals
		// - `super`, `self`, `invoker`
		return util::respond_null(ctx.conn, ctx.id);
	};

	let hover = match contents {
		Some(c) => Hover {
			contents: c,
			range: Some(ctx.src.make_range(span)),
		},
		None => {
			return util::respond_null(ctx.conn, ctx.id);
		}
	};

	let resp = Response {
		id: ctx.id,
		result: Some(serde_json::to_value(hover).unwrap()),
		error: None,
	};

	ctx.conn.sender.send(Message::Response(resp))?;

	Ok(())
}

#[must_use]
fn ident(ctx: &request::Context, token: SyntaxToken) -> Option<HoverContents> {
	let fspan = FileSpan {
		file_id: ctx.src.id,
		span: token.text_range(),
	};

	let Some(SymGraphValue::Symbol(sym_ix)) =
		ctx.core.ready.sym_graph.get(&SymGraphKey::Reference(fspan))
	else {
		#[cfg(debug_assertions)]
		debug!("Hover info miss: ZScript identifier refers to nothing.");

		return None;
	};

	let sym = match ctx.core.ready.symbol(*sym_ix) {
		OneOf::Left(sym) => sym,
		OneOf::Right(internal) => {
			let mut strings = vec![];

			for &doc in internal.docs {
				strings.push(MarkedString::String(doc.to_owned()));
			}

			strings.push(MarkedString::LanguageString(LanguageString {
				language: internal.lang.to_str().to_owned(),
				value: internal.decl.to_owned(),
			}));

			return Some(HoverContents::Array(strings));
		}
	};

	let (_, sym_project) = ctx.core.project_with(sym.id.file_id).unwrap();
	let origin = sym_project.files.get(&sym.id.file_id).unwrap();

	Some(HoverContents::Array(vec![MarkedString::LanguageString(
		LanguageString {
			language: sym.lang.to_str().to_owned(),
			value: origin.text[sym.crit_span].to_owned(),
		},
	)]))
}
