//! Handling for `textDocument/hover` requests.

use doomfront::zdoom::zscript::{Syn, SyntaxToken};
use lsp_server::{ErrorCode, Message, Response};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString};

use crate::{project, request, Core, Error, UnitResult};

use super::Datum;

pub(crate) fn req_hover(ctx: request::Context, params: HoverParams) -> UnitResult {
	let Some(parsed) = &ctx.sfile.parsed else {
		return Core::respond_null(ctx.conn, ctx.id);
	};

	let pos = params.text_document_position_params.position;

	let Some(token) = parsed.token_at::<Syn>(pos, &ctx.sfile.lndx) else {
		return Err(Error::Process {
			source: None,
			ctx: format!("invalid position {pos:#?}"),
		}.map_to_response(ctx.id, ErrorCode::InvalidParams));
	};

	let contents = if !token.kind().is_keyword() {
		req_hover_symbol(&ctx, token)
	} else {
		req_hover_keyword(token)
	};

	let hover = match contents {
		Some(c) => Hover {
			contents: c,
			range: None,
		},
		None => {
			ctx.conn.sender.send(Message::Response(Response {
				id: ctx.id,
				result: Some(serde_json::Value::Null),
				error: None,
			}))?;

			return Ok(());
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
fn req_hover_symbol(ctx: &request::Context, token: SyntaxToken) -> Option<HoverContents> {
	let Some(scopes) = super::prepare_scope_stack(ctx, &token) else {
		return None;
	};

	let iname_tgt_t = ctx.core.strings.type_name_nocase(token.text());
	let iname_tgt_v = ctx.core.strings.value_name_nocase(token.text());

	let Some(datum) = super::lookup_symbol(&scopes, [iname_tgt_t, iname_tgt_v]) else {
		return None;
	};

	let project::Datum::ZScript(dat_zs) = datum;

	let iname = match dat_zs {
		Datum::Class(dat_class) => dat_class.name,
		Datum::Value(dat_val) => dat_val.name,
		Datum::Enum(dat_enum) => dat_enum.name,
		Datum::MixinClass(dat_mixin) => dat_mixin.name,
		Datum::Struct(dat_struct) => dat_struct.name,
		Datum::Function(dat_fn) => dat_fn.name,
		Datum::Primitive(dat_prim) => dat_prim.name,
	};

	let name_string = ctx
		.core
		.strings
		.resolve_nocase(iname, |s| match dat_zs {
			// TODO:
			// - zscdoc support.
			// - Symbolic constants should have types inferred and presented.
			// - Variable values should be formatted with their type.
			// - Function signatures.
			Datum::Class(dat_class) => {
				if let Some(ancestor) = dat_class.parent {
					ctx.core
						.strings
						.resolve_nocase(ancestor, |s1| format!("class {s} : {s1}"))
						.unwrap()
				} else {
					format!("class {s}")
				}
			}
			Datum::Value(dat_val) => {
				if dat_val.mutable {
					s.clone().into_string()
				} else {
					format!("const {s}")
				}
			}
			Datum::Enum(_) => format!("enum {s}"),
			Datum::MixinClass(_) => format!("mixin class {s}"),
			Datum::Struct(_) => format!("struct {s}"),
			Datum::Function(_) => format!("{s}()"),
			Datum::Primitive(_) => s.clone().into_string(),
		})
		.unwrap();

	Some(HoverContents::Array(vec![MarkedString::LanguageString(
		LanguageString {
			language: "zscript".to_string(),
			value: name_string,
		},
	)]))
}

#[must_use]
fn req_hover_keyword(token: SyntaxToken) -> Option<HoverContents> {
	let (kw, contents) = match token.kind() {
		Syn::KwClass => (
			"class",
			&[
				"A class defines an object type within ZScript, and is most \
					of what you'll be creating within the language.",
			],
		),
		Syn::KwStruct => (
			"struct",
			&[
				"A structure is an object type that does not inherit from Object \
				and is not always — though occasionally is — a reference type, unlike classes.",
			],
		),
		_ => return None,
	};

	let mut ret = vec![];

	ret.push(MarkedString::LanguageString(LanguageString {
		language: "zscript".to_string(),
		value: kw.to_string(),
	}));

	for c in contents {
		ret.push(MarkedString::String(c.to_string()));
	}

	Some(HoverContents::Array(ret))
}
