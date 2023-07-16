use doomfront::{
	rowan::ast::AstNode,
	zdoom::zscript::{ast, Syn},
};
use lsp_server::{Message, Response};
use lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind};

use crate::{
	names::IName,
	project::{self, Scope},
	request, util, UnitResult,
};

use super::{
	ClassDatum, Datum, EnumDatum, FunctionDatum, FunctionSource, StructDatum, ValueDatum,
	ValueKind, ValueSource,
};

pub(crate) fn req_doc_symbols(ctx: request::Context) -> UnitResult {
	let parsed = ctx.sfile.parsed.as_ref().unwrap();
	let mut docsyms = vec![];

	for iname in parsed.symbols.iter().copied() {
		let datum = ctx.project.lookup_global(iname).unwrap();
		let project::Datum::ZScript(dat_zs) = datum;
		docsyms.push(doc_symbol(&ctx, ctx.project.globals(), iname, dat_zs));
	}

	ctx.conn.sender.send(Message::Response(Response {
		id: ctx.id,
		result: Some(serde_json::to_value(DocumentSymbolResponse::Nested(docsyms)).unwrap()),
		error: None,
	}))?;

	Ok(())
}

#[must_use]
fn doc_symbol(
	ctx: &request::Context,
	parent_scope: &Scope,
	iname: IName,
	datum: &Datum,
) -> DocumentSymbol {
	let name_string = ctx
		.core
		.strings
		.resolve_nocase(iname, |s| s.to_string())
		.unwrap();

	match datum {
		Datum::Class(dat_class) => doc_symbol_class(ctx, dat_class, name_string),
		Datum::Value(dat_val) => doc_symbol_value(ctx, dat_val, name_string),
		Datum::Enum(dat_enum) => doc_symbol_enum(ctx, parent_scope, dat_enum, name_string),
		Datum::MixinClass(_) => {
			let pos = datum.pos().unwrap();

			#[allow(deprecated)]
			DocumentSymbol {
				name: name_string,
				detail: None,
				kind: SymbolKind::INTERFACE,
				tags: None,
				deprecated: None,
				range: util::make_range(&ctx.sfile.lndx, pos.full_range),
				selection_range: util::make_range(&ctx.sfile.lndx, pos.name_range),
				children: None,
			}
		}
		Datum::Struct(dat_struct) => doc_symbol_struct(ctx, dat_struct, name_string),
		// User files can not declare primitives.
		Datum::Function(dat_func) => doc_symbol_function(ctx, dat_func, name_string),
		Datum::Primitive(_) => unreachable!(),
	}
}

#[must_use]
fn doc_symbol_class(ctx: &request::Context, datum: &ClassDatum, name: String) -> DocumentSymbol {
	let position = datum.position().unwrap();

	#[allow(deprecated)]
	DocumentSymbol {
		name,
		detail: Some({
			let mut ancestor = ": ".to_string();

			ctx.core
				.strings
				.resolve_nocase(datum.parent.unwrap(), |s| ancestor.push_str(s))
				.unwrap();

			ancestor
		}),
		kind: SymbolKind::CLASS,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, position.full_range),
		selection_range: util::make_range(&ctx.sfile.lndx, position.name_range),
		children: Some({
			let mut children = vec![];

			for (iname, innard) in datum.scope.iter() {
				let project::Datum::ZScript(innard_zs) = innard;
				children.push(doc_symbol(ctx, &datum.scope, *iname, innard_zs));
			}

			children
		}),
	}
}

#[must_use]
fn doc_symbol_enum(
	ctx: &request::Context,
	parent_scope: &Scope,
	datum: &EnumDatum,
	name: String,
) -> DocumentSymbol {
	let pos = datum.position().unwrap();

	#[allow(deprecated)]
	DocumentSymbol {
		name,
		detail: Some(format!(": {}", datum.underlying)),
		kind: SymbolKind::ENUM,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, pos.full_range),
		selection_range: util::make_range(&ctx.sfile.lndx, pos.name_range),
		children: Some({
			let mut children = vec![];

			for vname in datum.variants.iter().copied() {
				let datum = parent_scope.get(&vname).unwrap();
				let project::Datum::ZScript(variant) = datum;
				let Datum::Value(dat_val) = variant else { unreachable!() };
				let variant_name = ctx
					.core
					.strings
					.resolve_nocase(vname, |s| s.to_string())
					.unwrap();
				children.push(doc_symbol_value(ctx, dat_val, variant_name));
			}

			children
		}),
	}
}

#[must_use]
fn doc_symbol_function(
	ctx: &request::Context,
	datum: &FunctionDatum,
	name: String,
) -> DocumentSymbol {
	let pos = datum.position().unwrap();

	#[allow(deprecated)]
	DocumentSymbol {
		name,
		detail: Some({
			let FunctionSource::User { ast, .. } = &datum.source else {
				unreachable!()
			};

			let mut detail = String::new();

			for ret_t in ast.return_types().iter() {
				util::append_descendant_tokens(&mut detail, ret_t.syntax(), Syn::Whitespace);
				detail.push_str(", ");
			}

			let _ = detail.pop();
			let _ = detail.pop();

			let param_list = ast.param_list().unwrap();

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

			if datum.is_const {
				detail.push_str(" const");
			}

			detail
		}),
		kind: {
			if datum.is_static {
				SymbolKind::FUNCTION
			} else {
				SymbolKind::METHOD
			}
		},
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, pos.full_range),
		selection_range: util::make_range(&ctx.sfile.lndx, pos.name_range),
		children: None,
	}
}

#[must_use]
fn doc_symbol_struct(ctx: &request::Context, datum: &StructDatum, name: String) -> DocumentSymbol {
	let pos = datum.position().unwrap();

	#[allow(deprecated)]
	DocumentSymbol {
		name,
		detail: None,
		kind: SymbolKind::STRUCT,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, pos.full_range),
		selection_range: util::make_range(&ctx.sfile.lndx, pos.name_range),
		children: Some({
			let mut children = vec![];

			for (iname, innard) in datum.scope.iter() {
				let project::Datum::ZScript(innard_zs) = innard;
				children.push(doc_symbol(ctx, &datum.scope, *iname, innard_zs));
			}

			children
		}),
	}
}

#[must_use]
fn doc_symbol_value(ctx: &request::Context, datum: &ValueDatum, name: String) -> DocumentSymbol {
	let pos = datum.position().unwrap();

	let ValueSource::User { ast, .. } = &datum.source else {
		unreachable!()
	};

	let (kind, detail) = match datum.kind {
		ValueKind::_Local => (SymbolKind::VARIABLE, None),
		ValueKind::Field => (
			SymbolKind::FIELD,
			Some({
				let field = ast::FieldDecl::cast(ast.clone()).unwrap();

				util::descendant_tokens_to_string(
					field.type_spec().unwrap().syntax(),
					Syn::Whitespace,
				)
			}),
		),
		ValueKind::Constant => (
			SymbolKind::CONSTANT,
			Some({
				let constdef = ast::ConstDef::cast(ast.clone()).unwrap();

				util::descendant_tokens_to_string(
					constdef.initializer().unwrap().syntax(),
					Syn::Whitespace,
				)
			}),
		),
		ValueKind::EnumVariant => (SymbolKind::ENUM_MEMBER, {
			ast::EnumVariant::cast(ast.clone())
				.unwrap()
				.initializer()
				.map(|init| util::descendant_tokens_to_string(init.syntax(), Syn::Whitespace))
		}),
	};

	#[allow(deprecated)]
	DocumentSymbol {
		name,
		detail,
		kind,
		tags: None,
		deprecated: None,
		range: util::make_range(&ctx.sfile.lndx, pos.full_range),
		selection_range: util::make_range(&ctx.sfile.lndx, pos.name_range),
		children: None,
	}
}
