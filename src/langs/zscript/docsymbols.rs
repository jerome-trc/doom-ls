use doomfront::{
	rowan::ast::AstNode,
	zdoom::zscript::{ast, Syn, SyntaxNode},
};
use lsp_server::{Message, Response};
use lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind, SymbolTag};

use crate::{request, util, UnitResult};

pub(crate) fn req_doc_symbols(ctx: request::Context) -> UnitResult {
	let parsed = ctx.sfile.parsed.as_ref().unwrap();
	let mut docsyms = vec![];

	let cursor = SyntaxNode::new_root(parsed.green.clone());

	for child in cursor.children() {
		let Some(toplevel) = ast::TopLevel::cast(child) else {
			continue;
		};

		match toplevel {
			ast::TopLevel::ClassDef(classdef) => {
				if let Some(docsym) = doc_symbol_class(&ctx, classdef) {
					docsyms.push(docsym);
				}
			}
			ast::TopLevel::ClassExtend(classext) => {
				if let Some(docsym) = doc_symbol_classext(&ctx, classext) {
					docsyms.push(docsym);
				}
			}
			ast::TopLevel::ConstDef(constdef) => {
				if let Some(docsym) = doc_symbol_const(&ctx, constdef) {
					docsyms.push(docsym);
				}
			}
			ast::TopLevel::EnumDef(enumdef) => {
				if let Some(docsym) = doc_symbol_enum(&ctx, enumdef) {
					docsyms.push(docsym);
				}
			}
			ast::TopLevel::MixinClassDef(mixindef) => {
				if let Some(docsym) = doc_symbol_mixin(&ctx, mixindef) {
					docsyms.push(docsym);
				}
			}
			ast::TopLevel::StructDef(structdef) => {
				if let Some(docsym) = doc_symbol_struct(&ctx, structdef) {
					docsyms.push(docsym);
				}
			}
			ast::TopLevel::StructExtend(structext) => {
				if let Some(docsym) = doc_symbol_structext(&ctx, structext) {
					docsyms.push(docsym);
				}
			}
			ast::TopLevel::Include(_) | ast::TopLevel::Version(_) => {}
		}
	}

	ctx.conn.sender.send(Message::Response(Response {
		id: ctx.id,
		result: Some(serde_json::to_value(DocumentSymbolResponse::Nested(docsyms)).unwrap()),
		error: None,
	}))?;

	Ok(())
}

#[must_use]
fn doc_symbol_class(ctx: &request::Context, classdef: ast::ClassDef) -> Option<DocumentSymbol> {
	let Ok(name) = classdef.name() else {
		return None;
	};

	#[allow(deprecated)]
	let ret = DocumentSymbol {
		name: name.text().to_string(),
		detail: {
			let mut ancestor = ": ".to_string();

			if let Some(parent) = classdef.parent_class() {
				ancestor.push_str(parent.text());
			} else {
				ancestor.push_str("Object");
			}

			Some(ancestor)
		},
		kind: SymbolKind::CLASS,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, classdef.syntax().text_range()),
		selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
		children: {
			let mut children = vec![];

			for innard in classdef.innards() {
				doc_symbol_class_innard(ctx, &mut children, innard);
			}

			Some(children)
		},
	};

	Some(ret)
}

#[must_use]
fn doc_symbol_classext(
	ctx: &request::Context,
	classext: ast::ClassExtend,
) -> Option<DocumentSymbol> {
	let Ok(name) = classext.name() else {
		return None;
	};

	#[allow(deprecated)]
	let ret = DocumentSymbol {
		name: name.text().to_string(),
		detail: Some("extend class".to_string()),
		kind: SymbolKind::CLASS,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, classext.syntax().text_range()),
		selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
		children: {
			let mut children = vec![];

			for innard in classext.innards() {
				doc_symbol_class_innard(ctx, &mut children, innard);
			}

			Some(children)
		},
	};

	Some(ret)
}

fn doc_symbol_class_innard(
	ctx: &request::Context,
	docsyms: &mut Vec<DocumentSymbol>,
	innard: ast::ClassInnard,
) {
	match innard {
		ast::ClassInnard::Const(constdef) => {
			if let Some(docsym) = doc_symbol_const(ctx, constdef) {
				docsyms.push(docsym);
			}
		}
		ast::ClassInnard::Enum(enumdef) => {
			if let Some(docsym) = doc_symbol_enum(ctx, enumdef) {
				docsyms.push(docsym);
			}
		}
		ast::ClassInnard::Struct(structdef) => {
			if let Some(docsym) = doc_symbol_struct(ctx, structdef) {
				docsyms.push(docsym);
			}
		}
		ast::ClassInnard::StaticConst(sconst) => {
			if let Some(docsym) = doc_symbol_staticconst(ctx, sconst) {
				docsyms.push(docsym);
			}
		}
		ast::ClassInnard::Function(fndecl) => {
			if let Some(docsym) = doc_symbol_function(ctx, fndecl) {
				docsyms.push(docsym);
			}
		}
		ast::ClassInnard::Field(field) => {
			if let Some(dsyms) = doc_symbol_field(ctx, field) {
				for docsym in dsyms {
					docsyms.push(docsym);
				}
			}
		}
		ast::ClassInnard::Property(propdef) => {
			let Ok(name) = propdef.name() else {
				return;
			};

			#[allow(deprecated)]
			docsyms.push(DocumentSymbol {
				name: name.text().to_string(),
				detail: {
					let mut detail = ": ".to_string();

					for backing in propdef.backing_fields() {
						detail.push_str(backing.text());
						detail.push_str(", ");
					}

					detail.truncate(detail.len() - 2);
					Some(detail)
				},
				kind: SymbolKind::PROPERTY,
				tags: None,
				deprecated: None,
				range: util::make_range(&ctx.sfile.lndx, propdef.syntax().text_range()),
				selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
				children: None,
			});
		}
		ast::ClassInnard::Flag(flagdef) => {
			let Ok(name) = flagdef.name() else {
				return;
			};

			let Ok(backing) = flagdef.backing_field() else {
				return;
			};

			let Ok(bit) = flagdef.bit() else {
				return;
			};

			#[allow(deprecated)]
			docsyms.push(DocumentSymbol {
				name: name.text().to_string(),
				detail: Some(format!(": {}, {}", backing.text(), bit.syntax().text())),
				kind: SymbolKind::BOOLEAN,
				tags: None,
				deprecated: None,
				range: util::make_range(&ctx.sfile.lndx, flagdef.syntax().text_range()),
				selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
				children: None,
			});
		}
		ast::ClassInnard::States(states) => {
			for innard in states.innards() {
				match innard {
					ast::StatesInnard::Label(label) => {
						let range = util::make_range(&ctx.sfile.lndx, label.syntax().text_range());

						#[allow(deprecated)]
						docsyms.push(DocumentSymbol {
							name: util::syntaxtext_to_string(label.syntax().text()),
							detail: None,
							kind: SymbolKind::EVENT,
							tags: None,
							deprecated: None,
							range,
							selection_range: range,
							children: None,
						});
					}
					ast::StatesInnard::Flow(_) | ast::StatesInnard::State(_) => {}
				}
			}
		}
		// TODO: Do anything for these two?
		ast::ClassInnard::Default(_) | ast::ClassInnard::Mixin(_) => {}
	}
}

#[must_use]
fn doc_symbol_const(ctx: &request::Context, constdef: ast::ConstDef) -> Option<DocumentSymbol> {
	let Ok(name) = constdef.name() else {
		return None;
	};

	#[allow(deprecated)]
	let ret = DocumentSymbol {
		name: name.text().to_string(),
		detail: constdef
			.initializer()
			.ok()
			.map(|init| util::descendant_tokens_to_string(init.syntax(), Syn::Whitespace)),
		kind: SymbolKind::CONSTANT,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, constdef.syntax().text_range()),
		selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
		children: None,
	};

	Some(ret)
}

#[must_use]
fn doc_symbol_enum(ctx: &request::Context, enumdef: ast::EnumDef) -> Option<DocumentSymbol> {
	let Ok(name) = enumdef.name() else {
		return None;
	};

	#[allow(deprecated)]
	let ret = DocumentSymbol {
		name: name.text().to_string(),
		detail: Some({
			let u_t = enumdef
				.type_spec()
				.map(|(_token, t)| t)
				.unwrap_or(ast::EnumType::KwInt);

			format!(": {u_t}")
		}),
		kind: SymbolKind::CONSTANT,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, enumdef.syntax().text_range()),
		selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
		children: {
			let mut children = vec![];

			for variant in enumdef.variants() {
				let vname = variant.name();

				children.push(DocumentSymbol {
					name: vname.text().to_string(),
					detail: {
						variant.initializer().map(|init| {
							util::descendant_tokens_to_string(init.syntax(), Syn::Whitespace)
						})
					},
					kind: SymbolKind::ENUM_MEMBER,
					tags: None,
					deprecated: None,
					range: util::make_range(&ctx.sfile.lndx, variant.syntax().text_range()),
					selection_range: util::make_range(&ctx.sfile.lndx, vname.text_range()),
					children: None,
				});
			}

			Some(children)
		},
	};

	Some(ret)
}

#[must_use]
fn doc_symbol_field<'a>(
	ctx: &'a request::Context,
	field: ast::FieldDecl,
) -> Option<impl Iterator<Item = DocumentSymbol> + 'a> {
	let Ok(type_spec) = field.type_spec() else {
		return None;
	};

	let ret = field.names().map(move |varname| {
		#[allow(deprecated)]
		DocumentSymbol {
			name: varname.ident().text().to_string(),
			detail: Some(util::descendant_tokens_to_string(
				type_spec.syntax(),
				Syn::Whitespace,
			)),
			kind: SymbolKind::FIELD,
			tags: None,
			deprecated: None,
			range: util::make_range(&ctx.sfile.lndx, field.syntax().text_range()),
			selection_range: util::make_range(&ctx.sfile.lndx, varname.syntax().text_range()),
			children: None,
		}
	});

	Some(ret)
}

#[must_use]
fn doc_symbol_function(
	ctx: &request::Context,
	fndecl: ast::FunctionDecl,
) -> Option<DocumentSymbol> {
	let Ok(param_list) = fndecl.param_list() else {
		return None;
	};

	let name = fndecl.name();

	let mut is_static = false;
	let mut is_deprecated = false;

	for qual in fndecl.qualifiers().iter() {
		match qual {
			ast::MemberQual::Deprecation(_) => is_deprecated = true,
			ast::MemberQual::Static(_) => is_static = true,
			_ => {}
		}
	}

	#[allow(deprecated)]
	let ret = DocumentSymbol {
		name: name.text().to_string(),
		detail: {
			let mut detail = String::new();

			for ret_t in fndecl.return_types().iter() {
				util::append_descendant_tokens(&mut detail, ret_t.syntax(), Syn::Whitespace);
				detail.push_str(", ");
			}

			detail.truncate(detail.len() - 2);
			detail.push(' ');

			if param_list.is_empty() {
				detail.push_str("()");
			} else if param_list.is_void() {
				detail.push_str("(void)");
			} else {
				detail.push('(');

				for param in param_list.iter() {
					util::append_descendant_tokens(&mut detail, param.syntax(), Syn::Whitespace);
					detail.push_str(", ");
				}

				let _ = detail.pop();
				let _ = detail.pop();

				detail.push(')');
			}

			if fndecl.is_const() {
				detail.push_str(" const");
			}

			Some(detail)
		},
		kind: if is_static {
			SymbolKind::FUNCTION
		} else {
			SymbolKind::METHOD
		},
		tags: {
			if is_deprecated {
				Some(vec![SymbolTag::DEPRECATED])
			} else {
				None
			}
		},
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, fndecl.syntax().text_range()),
		selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
		children: None,
	};

	Some(ret)
}

#[must_use]
fn doc_symbol_mixin(
	ctx: &request::Context,
	mixindef: ast::MixinClassDef,
) -> Option<DocumentSymbol> {
	let Ok(name) = mixindef.name() else {
		return None;
	};

	#[allow(deprecated)]
	let ret = DocumentSymbol {
		name: name.text().to_string(),
		detail: None,
		kind: SymbolKind::INTERFACE,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, mixindef.syntax().text_range()),
		selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
		children: {
			let mut children = vec![];

			for innard in mixindef.innards() {
				doc_symbol_class_innard(ctx, &mut children, innard);
			}

			Some(children)
		},
	};

	Some(ret)
}

#[must_use]
fn doc_symbol_staticconst(
	ctx: &request::Context,
	sconst: ast::StaticConstStat,
) -> Option<DocumentSymbol> {
	let Ok(name) = sconst.name() else {
		return None;
	};

	#[allow(deprecated)]
	let ret = DocumentSymbol {
		name: name.text().to_string(),
		detail: None,
		kind: SymbolKind::ARRAY,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, sconst.syntax().text_range()),
		selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
		children: None,
	};

	Some(ret)
}

#[must_use]
fn doc_symbol_struct(ctx: &request::Context, structdef: ast::StructDef) -> Option<DocumentSymbol> {
	let Ok(name) = structdef.name() else {
		return None;
	};

	#[allow(deprecated)]
	let ret = DocumentSymbol {
		name: name.text().to_string(),
		detail: None,
		kind: SymbolKind::STRUCT,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, structdef.syntax().text_range()),
		selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
		children: Some({
			let mut children = vec![];

			for innard in structdef.innards() {
				doc_symbol_struct_innard(ctx, &mut children, innard);
			}

			children
		}),
	};

	Some(ret)
}

#[must_use]
fn doc_symbol_structext(
	ctx: &request::Context,
	structext: ast::StructExtend,
) -> Option<DocumentSymbol> {
	let Ok(name) = structext.name() else {
		return None;
	};

	#[allow(deprecated)]
	let ret = DocumentSymbol {
		name: name.text().to_string(),
		detail: Some("extend struct".to_string()),
		kind: SymbolKind::STRUCT,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, structext.syntax().text_range()),
		selection_range: util::make_range(&ctx.sfile.lndx, name.text_range()),
		children: {
			let mut children = vec![];

			for innard in structext.innards() {
				doc_symbol_struct_innard(ctx, &mut children, innard);
			}

			Some(children)
		},
	};

	Some(ret)
}

fn doc_symbol_struct_innard(
	ctx: &request::Context,
	docsyms: &mut Vec<DocumentSymbol>,
	innard: ast::StructInnard,
) {
	match innard {
		ast::StructInnard::Const(constdef) => {
			if let Some(docsym) = doc_symbol_const(ctx, constdef) {
				docsyms.push(docsym);
			}
		}
		ast::StructInnard::Enum(enumdef) => {
			if let Some(docsym) = doc_symbol_enum(ctx, enumdef) {
				docsyms.push(docsym);
			}
		}
		ast::StructInnard::StaticConst(sconst) => {
			if let Some(docsym) = doc_symbol_staticconst(ctx, sconst) {
				docsyms.push(docsym);
			}
		}
		ast::StructInnard::Function(fndecl) => {
			if let Some(docsym) = doc_symbol_function(ctx, fndecl) {
				docsyms.push(docsym);
			}
		}
		ast::StructInnard::Field(field) => {
			if let Some(dsyms) = doc_symbol_field(ctx, field) {
				for docsym in dsyms {
					docsyms.push(docsym);
				}
			}
		}
	}
}
