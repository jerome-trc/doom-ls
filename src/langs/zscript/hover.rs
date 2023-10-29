//! Handling for `textDocument/hover` requests.

mod keyword;

use doomfront::{
	rowan::ast::AstNode,
	zdoom::zscript::{ast, Syn, SyntaxToken},
};
use lsp_server::{Message, Response};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString};
use tracing::debug;

use crate::{
	data::{FileSpan, SymGraphKey, SymGraphVal, Symbol},
	langs::LangId,
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

	let Some(SymGraphVal::Symbol(sym_ptr)) =
		ctx.core.ready.sym_graph.get(&SymGraphKey::Reference(fspan))
	else {
		#[cfg(debug_assertions)]
		debug!("Hover info miss: ZScript identifier refers to nothing.");

		return None;
	};

	let mut strings = vec![];

	match sym_ptr.as_ref().unwrap() {
		Symbol::User(u_sym) => {
			let origin_src = ctx.core.file_with(u_sym);
			let syn_node = origin_src.node_covering(u_sym.id.span);

			if u_sym.lang == LangId::ZScript {
				if let Some(documentable) = ast::Documentable::cast(syn_node.clone()) {
					let mut doc_string = String::new();

					for doc in documentable.docs() {
						doc_string.push_str(doc.text_trimmed());
						doc_string.push(' ');
					}

					doc_string.pop();

					strings.push(MarkedString::String(doc_string));
				}

				// Frontend symbols come from `VarName` nodes, but their parent
				// `FielDecl` nodes are what get documented.

				if let Some(documentable) = syn_node.parent().and_then(ast::Documentable::cast) {
					let mut doc_string = String::new();

					for doc in documentable.docs() {
						doc_string.push_str(doc.text_trimmed());
						doc_string.push(' ');
					}

					doc_string.pop();

					strings.push(MarkedString::String(doc_string));
				}
			}

			if let Some(t) = ctx.core.decl_text(sym_ptr, u_sym) {
				strings.push(MarkedString::LanguageString(LanguageString {
					language: u_sym.lang.to_str().to_owned(),
					value: t,
				}));
			}
		}
		Symbol::Internal(in_sym) => {
			for &doc in in_sym.docs {
				strings.push(MarkedString::String(doc.to_owned()));
			}

			strings.push(MarkedString::LanguageString(LanguageString {
				language: in_sym.lang.to_str().to_owned(),
				value: in_sym.decl.to_owned(),
			}));
		}
	}

	Some(HoverContents::Array(strings))
}
