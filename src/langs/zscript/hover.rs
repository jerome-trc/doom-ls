//! Handling for `textDocument/hover` requests.

use doomfront::{
	rowan::ast::AstNode,
	zdoom::{
		ast::LitToken,
		zscript::{ast, Syn, SyntaxElem, SyntaxNode, SyntaxToken},
	},
};
use lsp_server::{ErrorCode, Message, Response};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString};

use crate::{project, request, util, Core, Error, UnitResult};

use super::{
	ClassDatum, ClassSource, Datum, EnumDatum, EnumSource, FunctionDatum, FunctionSource,
	MixinClassDatum, MixinClassSource, StructDatum, StructSource, ValueKind, ValueSource,
};

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

	let contents = if token.kind() == Syn::Ident {
		req_hover_ident(&ctx, token)
	} else if token.kind() == Syn::NameLit {
		req_hover_namelit(&ctx, token)
	} else if token.kind().is_keyword() {
		req_hover_keyword(token)
	} else {
		return Core::respond_null(ctx.conn, ctx.id);
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
fn req_hover_ident(ctx: &request::Context, token: SyntaxToken) -> Option<HoverContents> {
	let Some(scopes) = super::prepare_scope_stack(ctx, &token) else {
		return None;
	};

	let iname_tgt_t = ctx.core.strings.type_name_nocase(token.text());
	let iname_tgt_v = ctx.core.strings.value_name_nocase(token.text());

	let Some(datum) = super::lookup_symbol(&scopes, [iname_tgt_t, iname_tgt_v]) else {
		return None;
	};

	let project::Datum::ZScript(dat_zs) = datum;
	Some(symbol_hover(ctx, dat_zs))
}

#[must_use]
fn req_hover_namelit(ctx: &request::Context, token: SyntaxToken) -> Option<HoverContents> {
	let scopes = ctx.core.scope_stack();
	let lit = LitToken::new(token);
	let text = lit.name().unwrap();
	let iname = ctx.core.strings.type_name_nocase(text);

	let Some(datum) = super::lookup_symbol(&scopes, [iname]) else {
		return None;
	};

	let project::Datum::ZScript(dat_zs) = datum;
	Some(symbol_hover(ctx, dat_zs))
}

fn symbol_hover(ctx: &request::Context, datum: &Datum) -> HoverContents {
	let iname = match datum {
		Datum::Class(dat_class) => dat_class.name,
		Datum::Value(dat_val) => dat_val.name,
		Datum::Enum(dat_enum) => dat_enum.name,
		Datum::MixinClass(dat_mixin) => dat_mixin.name,
		Datum::Struct(dat_struct) => dat_struct.name,
		Datum::Function(dat_fn) => dat_fn.name,
		Datum::Primitive(dat_prim) => dat_prim.name,
	};

	let (code, docs) = ctx
		.core
		.strings
		.resolve_nocase(iname, |s| match datum {
			Datum::Class(dat_class) => format_class_info(ctx, dat_class, s),
			Datum::Value(dat_val) => match &dat_val.source {
				ValueSource::User { ast, .. } => match dat_val.kind {
					ValueKind::_Local => format_local_info(ctx, ast),
					ValueKind::Field => format_field_info(ctx, ast),
					ValueKind::Constant => format_constant_info(ctx, ast, s),
					ValueKind::EnumVariant => format_enum_variant_info(ctx, ast),
				},
				ValueSource::Native { docs, decl } => (decl.to_string(), docs.to_string()),
			},
			Datum::Enum(dat_enum) => format_enum_info(ctx, dat_enum, s),
			Datum::MixinClass(dat_mixin) => format_mixin_info(ctx, dat_mixin, s),
			Datum::Struct(dat_struct) => format_struct_info(ctx, dat_struct, s),
			Datum::Function(dat_fn) => format_function_info(ctx, dat_fn, s),
			Datum::Primitive(dat_prim) => (s.clone().into_string(), dat_prim.doc.to_string()),
		})
		.unwrap();

	let mut contents = vec![MarkedString::LanguageString(LanguageString {
		language: "zscript".to_string(),
		value: code,
	})];

	if !docs.is_empty() {
		contents.push(MarkedString::String(docs));
	}

	HoverContents::Array(contents)
}

#[must_use]
fn format_class_info(ctx: &request::Context, datum: &ClassDatum, name: &str) -> (String, String) {
	match &datum.source {
		ClassSource::User { ast, .. } => {
			let mut decl = format!("class {name}");
			let mut docs = String::new();

			if let Some(ancestor) = datum.parent {
				ctx.core
					.strings
					.resolve_nocase(ancestor, |s1| {
						decl.push_str(" : ");
						decl.push_str(s1);
					})
					.unwrap();
			}

			for qual in ast.qualifiers() {
				decl.push(' ');

				match qual {
					ast::ClassQual::Replaces(clause) => {
						util::append_syntaxtext(&mut decl, clause.syntax().text());
					}
					ast::ClassQual::Abstract(_) => decl.push_str("abstract"),
					ast::ClassQual::Play(_) => decl.push_str("play"),
					ast::ClassQual::Ui(_) => decl.push_str("ui"),
					ast::ClassQual::Native(_) => decl.push_str("native"),
					ast::ClassQual::Version(version) => {
						util::append_syntaxtext(&mut decl, version.syntax().text());
					}
				}
			}

			for doc in ast.docs() {
				docs.push_str(doc.text_trimmed());
			}

			(decl, docs)
		}
		ClassSource::Native { docs: doc, decl } => (decl.to_string(), doc.to_string()),
	}
}

#[must_use]
fn format_constant_info(_: &request::Context, node: &SyntaxNode, name: &str) -> (String, String) {
	let constdef = ast::ConstDef::cast(node.clone()).unwrap();
	let mut decl = format!("const {name}");
	let mut docs = String::new();

	if let Ok(init) = constdef.initializer() {
		decl.push_str(" = ");
		util::append_syntaxtext(&mut decl, init.syntax().text());
	}

	for doc in constdef.docs() {
		docs.push_str(doc.text_trimmed());
	}

	(decl, docs)
}

#[must_use]
fn format_enum_info(_: &request::Context, datum: &EnumDatum, name: &str) -> (String, String) {
	match &datum.source {
		EnumSource::User { ast, .. } => {
			let mut docs = String::new();

			for doc in ast.docs() {
				docs.push_str(doc.text_trimmed());
			}

			(format!("enum {name} : {t}", t = datum.underlying), docs)
		}
		EnumSource::Native { doc, decl } => (decl.to_string(), doc.to_string()),
	}
}

#[must_use]
fn format_enum_variant_info(_: &request::Context, node: &SyntaxNode) -> (String, String) {
	let variant = ast::EnumVariant::cast(node.clone()).unwrap();

	let mut decl = String::new();
	let mut docs = String::new();

	decl.push_str(variant.name().text());

	if let Some(init) = variant.initializer() {
		decl.push_str(" = ");

		init.syntax().text().for_each_chunk(|chunk| {
			decl.push_str(chunk);
		});
	}

	for doc in variant.docs() {
		docs.push_str(doc.text_trimmed());
	}

	(decl, docs)
}

#[must_use]
fn format_field_info(_: &request::Context, node: &SyntaxNode) -> (String, String) {
	let field = ast::FieldDecl::cast(node.clone()).unwrap();

	let mut decl = String::new();
	let mut docs = String::new();

	let elems = field.syntax().children_with_tokens().skip_while(|elem| {
		elem.as_token()
			.is_some_and(|token| token.kind() == Syn::DocComment || token.kind().is_trivia())
	});

	for elem in elems {
		match elem {
			SyntaxElem::Token(token) => {
				decl.push_str(token.text());
			}
			SyntaxElem::Node(node) => {
				util::append_syntaxtext(&mut decl, node.text());
			}
		}
	}

	for doc in field.docs() {
		docs.push_str(doc.text_trimmed());
	}

	(decl, docs)
}

#[must_use]
fn format_function_info(
	_: &request::Context,
	datum: &FunctionDatum,
	name: &str,
) -> (String, String) {
	match &datum.source {
		FunctionSource::User { ast, .. } => {
			let mut decl = String::new();
			let mut docs = String::new();

			let quals = ast.qualifiers();
			let ret_types = ast.return_types();
			let param_list = ast.param_list().unwrap();

			util::append_syntaxtext(&mut decl, quals.syntax().text());
			util::append_syntaxtext(&mut decl, ret_types.syntax().text());
			decl.push_str(name);
			util::append_syntaxtext(&mut decl, param_list.syntax().text());

			if datum.is_const {
				decl.push_str(" const");
			}

			for doc in ast.docs() {
				docs.push_str(doc.text_trimmed());
			}

			(decl, docs)
		}
		FunctionSource::Native { doc, signature } => (signature.to_string(), doc.to_string()),
	}
}

#[must_use]
fn format_local_info(_: &request::Context, node: &SyntaxNode) -> (String, String) {
	(util::syntaxtext_to_string(node.text()), String::new())
}

#[must_use]
fn format_mixin_info(
	_: &request::Context,
	datum: &MixinClassDatum,
	name: &str,
) -> (String, String) {
	match &datum.source {
		MixinClassSource::User { ast, .. } => {
			let mut docs = String::new();

			for doc in ast.docs() {
				docs.push_str(doc.text_trimmed());
			}

			(format!("mixin class {name}"), docs)
		}
		MixinClassSource::Native { decl, doc } => (decl.to_string(), doc.to_string()),
	}
}

#[must_use]
fn format_struct_info(_: &request::Context, datum: &StructDatum, name: &str) -> (String, String) {
	match &datum.source {
		StructSource::User { ast, .. } => {
			let mut decl = format!("struct {name}");
			let mut docs = String::new();

			for qual in ast.qualifiers() {
				decl.push(' ');

				match qual {
					ast::StructQual::Play(_) => decl.push_str("play"),
					ast::StructQual::Ui(_) => decl.push_str("ui"),
					ast::StructQual::Native(_) => decl.push_str("native"),
					ast::StructQual::ClearScope(_) => decl.push_str("clearscope"),
					ast::StructQual::Version(version) => {
						util::append_syntaxtext(&mut decl, version.syntax().text());
					}
				}
			}

			for doc in ast.docs() {
				docs.push_str(doc.text_trimmed());
			}

			(decl, docs)
		}
		StructSource::Native { decl, doc } => (decl.to_string(), doc.to_string()),
	}
}

#[must_use]
fn req_hover_keyword(token: SyntaxToken) -> Option<HoverContents> {
	let (kw, contents) = match token.kind() {
		Syn::KwAbstract => (
			"abstract",
			&["A class marked `abstract` cannot be instantiated with `new`."],
		),
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
		// TODO
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
