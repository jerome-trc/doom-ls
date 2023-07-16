//! Request and notification handling for ZDoom's [ZScript] language.
//!
//! [ZScript]: doomfront::zdoom::zscript

pub(crate) mod highlight;
mod hover;
pub(crate) mod sema;

use std::{
	ops::ControlFlow,
	path::{Path, PathBuf},
};

pub(crate) use doomfront::zdoom::zscript::Syn;
use doomfront::{
	rowan::{ast::AstNode, GreenNode, Language, NodeOrToken, TextRange, TextSize, WalkEvent},
	zdoom::{
		ast::LitToken,
		zscript::{ast, SyntaxNode, SyntaxToken},
	},
	ParseError,
};
use lsp_server::{Connection, ErrorCode, Message, RequestId, Response};
use lsp_types::{
	Diagnostic, DiagnosticSeverity, DocumentSymbol, DocumentSymbolResponse, GotoDefinitionResponse,
	Location, Position, SemanticTokens, SemanticTokensRangeResult, SemanticTokensResult,
	SymbolKind,
};
use parking_lot::Mutex;
use rayon::prelude::*;

use crate::{
	lines::{LineCol, LineIndex, TextDelta},
	names::IName,
	paths::PathInterner,
	project::{self, ParseErrors, ParsedFile, Project, Scope, SourceFile, StackedScope},
	request, util, Core, Error, ErrorBox, LangId, UnitResult,
};

pub(crate) use self::{hover::req_hover, sema::*};

// Request handling ////////////////////////////////////////////////////////////

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

			match req_goto_ident(&ctx, token) {
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
		Syn::NameLit => match req_goto_name(&ctx, token) {
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

/// Returns:
/// - `Continue` if the symbol hasn't been found.
/// - `Break(Ok)` if the symbol was resolved successfully.
fn req_goto_ident(
	ctx: &request::Context,
	token: SyntaxToken,
) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
	let iname_tgt_t = ctx.core.strings.type_name_nocase(token.text());
	let iname_tgt_v = ctx.core.strings.value_name_nocase(token.text());

	let Some(scopes) = prepare_scope_stack(ctx, &token) else {
		return ControlFlow::Continue(());
	};

	location_by_inames(ctx, &scopes, [iname_tgt_t, iname_tgt_v], &token)
}

fn req_goto_name(
	ctx: &request::Context,
	token: SyntaxToken,
) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
	let scopes = ctx.core.scope_stack();
	let lit = LitToken::new(token);
	let text = lit.name().unwrap();
	let iname = ctx.core.strings.type_name_nocase(text);
	location_by_inames(ctx, &scopes, [iname], lit.syntax())
}

pub(crate) fn req_references(
	core: &Core,
	conn: &Connection,
	id: RequestId,
	pos: Position,
	ix_project: usize,
	sfile: &SourceFile,
) -> UnitResult {
	let parsed = sfile.parsed.as_ref().unwrap();
	let cursor = SyntaxNode::new_root(parsed.green.clone());

	let linecol = LineCol {
		line: pos.line,
		col: pos.character,
	};

	let Some(boffs) = sfile.lndx.offset(linecol) else {
		return Err(Error::Process {
			source: None,
			ctx: format!("failed to find token at position {linecol:#?}")
		}.map_to_response(id, ErrorCode::InvalidParams));
	};

	let Some(token) = cursor.token_at_offset(boffs).next() else {
		return Err(Error::Process {
			source: None,
			ctx: format!("failed to find token at position {linecol:#?}")
		}.map_to_response(id, ErrorCode::InvalidParams));
	};

	if token.kind() != Syn::Ident {
		// TODO:
		// - Core types (numeric primitives, array, map, et cetera).
		// - `Syn::NameLit`, often identifying a class.
		// - `Syn::StringLit`, which may be coerced to `name` and identify a class.
		// Also support LANGUAGE IDs, GLDEFS, maybe even CVar names.
		// - `Syn::KwSuper`; try to go to a parent class definition.
		return Core::respond_null(conn, id);
	}

	let subvecs = Mutex::new(vec![]);
	let ident = token.text();

	for i in (0..=ix_project).rev() {
		let project = &core.projects[i];
		let paths = project.paths();

		project.all_files_par().for_each(|(file_id, sfile)| {
			let path = paths.resolve_native(file_id).unwrap();
			subvecs.lock().push(references_to(sfile, path, ident));
		});
	}

	let locations = subvecs
		.into_inner()
		.into_iter()
		.flatten()
		.collect::<Vec<_>>();

	conn.sender.send(Message::Response(Response {
		id,
		result: Some(serde_json::to_value(locations).unwrap()),
		error: None,
	}))?;

	Ok(())
}

#[must_use]
fn references_to(sfile: &SourceFile, path: &Path, ident: &str) -> Vec<Location> {
	let Some(parsed) = &sfile.parsed else { return vec![]; };
	let cursor = SyntaxNode::new_root(parsed.green.clone());
	let mut ret = vec![];

	for w_ev in cursor.preorder_with_tokens() {
		let WalkEvent::Enter(elem) = w_ev else { continue; };
		let NodeOrToken::Token(token) = elem else { continue; };

		if token.text().eq_ignore_ascii_case(ident) {
			let uri = util::path_to_uri(path).unwrap();
			let start_lc = sfile.lndx.line_col(token.text_range().start());
			let end_lc = sfile.lndx.line_col(token.text_range().end());

			ret.push(Location {
				uri,
				range: lsp_types::Range {
					start: Position {
						line: start_lc.line,
						character: start_lc.col,
					},
					end: Position {
						line: end_lc.line,
						character: end_lc.col,
					},
				},
			});
		}
	}

	ret
}

pub(crate) fn req_semtokens_full(ctx: request::Context) -> UnitResult {
	let Some(parsed) = &ctx.sfile.parsed else { unreachable!() };
	let cursor = SyntaxNode::new_root(parsed.green.clone());
	let highlights = highlight::Context::new(&ctx, &ctx.sfile.lndx).traverse(cursor);

	let resp = Response {
		id: ctx.id,
		result: Some(
			serde_json::to_value(SemanticTokensResult::Tokens(SemanticTokens {
				result_id: None,
				data: highlights,
			}))
			.unwrap(),
		),
		error: None,
	};

	ctx.conn.sender.send(Message::Response(resp))?;
	Ok(())
}

pub(crate) fn req_semtokens_range(ctx: request::Context, range: lsp_types::Range) -> UnitResult {
	let Some(parsed) = &ctx.sfile.parsed else { unreachable!() };
	let cursor = SyntaxNode::new_root(parsed.green.clone());

	let lc_start = LineCol {
		line: range.start.line,
		col: range.end.character,
	};

	let lc_end = LineCol {
		line: range.end.line,
		col: range.end.character,
	};

	let Some(start) = ctx.sfile.lndx.offset(lc_start) else {
		return Err(Error::Process {
			source: None,
			ctx: format!("invalid position {lc_start:#?}")
		}.map_to_response(ctx.id, ErrorCode::InvalidParams));
	};

	let Some(end) = ctx.sfile.lndx.offset(lc_end) else {
		return Err(Error::Process {
			source: None,
			ctx: format!("invalid position {lc_end:#?}")
		}.map_to_response(ctx.id, ErrorCode::InvalidParams));
	};

	let node = match cursor.covering_element(TextRange::new(start, end)) {
		NodeOrToken::Node(node) => node,
		NodeOrToken::Token(token) => token.parent().unwrap(),
	};

	let highlights = highlight::Context::new(&ctx, &ctx.sfile.lndx).traverse(node);

	let resp = Response {
		id: ctx.id,
		result: Some(
			serde_json::to_value(SemanticTokensRangeResult::Tokens(SemanticTokens {
				result_id: None,
				data: highlights,
			}))
			.unwrap(),
		),
		error: None,
	};

	ctx.conn.sender.send(Message::Response(resp))?;
	Ok(())
}

#[must_use]
fn prepare_scope_stack(ctx: &request::Context, token: &SyntaxToken) -> Option<Vec<StackedScope>> {
	// To understand this, consider an example:
	// - `token` is an identifier for a variable in a function in a class.
	// - `global_node` is the class' AST node.
	// - `name` is the class' identifier.
	// - `datum` is the class' semantic representation object.
	// - `datum.add_scopes_containing` adds the class scope and the function's body.

	let Some(global_node) = sema::global_containing(token) else {
		return None;
	};

	let Some(name) = sema::top_level_name(global_node) else {
		return None;
	};

	let iname_tl = ctx.core.strings.type_name_nocase(name.text());

	let Some(datum) = ctx.project.lookup_global(iname_tl) else {
		return None;
	};

	let mut scopes = ctx.core.scope_stack();
	datum.add_scopes_containing(&mut scopes, token.text_range());

	Some(scopes)
}

#[must_use]
fn lookup_symbol<const N: usize>(
	scopes: &[StackedScope],
	inames: [IName; N],
) -> Option<&project::Datum> {
	for scope in scopes.iter().rev() {
		if let Some(d) = inames.into_iter().find_map(|n| scope.inner.get(&n)) {
			return Some(d);
		}
	}

	None
}

#[must_use]
fn location_by_inames<const N: usize>(
	ctx: &request::Context,
	scopes: &[StackedScope],
	inames: [IName; N],
	token: &SyntaxToken,
) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
	for scope in scopes.iter().rev() {
		let Some(d) = inames.into_iter().find_map(|n| {
			scope.inner.get(&n)
		}) else {
			continue;
		};

		let Some(i) = scope.ix_project else {
			return ControlFlow::Continue(());
		};

		let project = &ctx.core.projects[i];

		let datpos = match d {
			project::Datum::ZScript(dat_zs) => {
				if let Some(dpos) = dat_zs.pos() {
					dpos
				} else {
					return ControlFlow::Break(Ok(GotoDefinitionResponse::Array(vec![])));
				}
			}
		};

		let path = project.paths().resolve_native(datpos.file).unwrap();
		let sfile = project.get_file(datpos.file).unwrap();

		return match util::make_location(
			&sfile.lndx,
			path,
			datpos.name_range.start(),
			token.text().len(),
		) {
			Ok(l) => ControlFlow::Break(Ok(GotoDefinitionResponse::Scalar(l))),
			Err(err) => ControlFlow::Break(Err(Box::new(err))),
		};
	}

	ControlFlow::Continue(())
}

// Notification handling ///////////////////////////////////////////////////////

/// Uses the [`rayon`] global thread pool.
/// In the `Err` case, the root itself could not be read.
/// `zs_root` will always have the file stem `ZSCRIPT` (any ASCII casing),
/// with one or no extension.
pub(crate) fn rebuild_include_tree(
	project: &mut Project,
	zs_root: PathBuf,
) -> std::io::Result<Vec<Diagnostic>> {
	#[derive(Debug, Clone, Copy)]
	struct Context<'p> {
		paths: &'p PathInterner,
		output: &'p Mutex<Vec<StagedFile>>,
		diags: &'p Mutex<Vec<Diagnostic>>,
	}

	#[derive(Debug)]
	struct StagedFile {
		path: PathBuf,
		source: SourceFile,
	}

	fn recur(ctx: Context, base: &Path, path: PathBuf, text: String) {
		let (green, errors) = doomfront::parse(
			&text,
			doomfront::zdoom::zscript::parse::file,
			// TODO: Either user-configurable or from the root.
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		)
		.into_inner();

		let lndx = LineIndex::new(&text);
		let incpaths = collect_include_paths(ctx, &lndx, green.clone());

		incpaths.into_par_iter().for_each(|(incpath, range)| {
			let complete = match base.join(&incpath).canonicalize() {
				Ok(c) => c,
				Err(err) => {
					tracing::error!(
						"Failed to canonicalize include path: `{}` - {err}",
						incpath.display()
					);
					return;
				}
			};

			let Some(file_id) = ctx
				.paths
				.get_nocase(&complete)
				 else {
					tracing::error!("Failed to get interned ZDoom path: `{}`", complete.display());
					return;
				};

			let real_path = ctx.paths.resolve_native(file_id).unwrap();

			let text = match std::fs::read_to_string(real_path) {
				Ok(s) => s,
				Err(err) => {
					ctx.diags.lock().push(Diagnostic {
						range: util::make_range(&lndx, range),
						severity: Some(DiagnosticSeverity::ERROR),
						code: None,
						code_description: None,
						source: None,
						message: format!("Failed to read file: `{}` - {err}", real_path.display()),
						related_information: None,
						tags: None,
						data: None,
					});

					return;
				}
			};

			let base = complete.parent().unwrap();
			recur(ctx, base, real_path.to_path_buf(), text);
		});

		ctx.output.lock().push(StagedFile {
			path,
			source: SourceFile {
				lang: LangId::ZScript,
				text,
				lndx,
				parsed: Some(ParsedFile {
					green,
					symbols: vec![],
					errors: ParseErrors::ZScript(errors),
				}),
			},
		});
	}

	#[must_use]
	fn collect_include_paths(
		ctx: Context,
		lndx: &LineIndex,
		green: GreenNode,
	) -> Vec<(PathBuf, TextRange)> {
		let cursor = SyntaxNode::new_root(green);

		cursor
			.children()
			.filter_map(|child| {
				if child.kind() != Syn::IncludeDirective {
					return None;
				}

				// This is infallible, since an include directive must have
				// at least an `#include` token.
				let last_token = child.last_token().unwrap();

				if last_token.kind() != Syn::StringLit {
					ctx.diags.lock().push(Diagnostic {
						range: util::make_range(lndx, last_token.text_range()),
						severity: Some(DiagnosticSeverity::ERROR),
						code: None,
						code_description: None,
						source: None,
						message: format!("Expected a string, found: {:#?}", last_token.kind()),
						related_information: None,
						tags: None,
						data: None,
					});
					return None;
				};

				let text = last_token.text();

				if !text.is_empty() {
					Some((
						PathBuf::from(&text[1..(text.len() - 1)]),
						last_token.text_range(),
					))
				} else {
					ctx.diags.lock().push(Diagnostic {
						range: util::make_range(lndx, last_token.text_range()),
						severity: Some(DiagnosticSeverity::ERROR),
						code: None,
						code_description: None,
						source: None,
						message: "Expected a path, found an empty string".to_string(),
						related_information: None,
						tags: None,
						data: None,
					});
					None
				}
			})
			.collect()
	}

	let text = std::fs::read_to_string(&zs_root)?;
	let output = Mutex::new(vec![]);
	let diags = Mutex::new(vec![]);

	let ctx = Context {
		paths: project.paths(),
		output: &output,
		diags: &diags,
	};

	recur(ctx, project.root(), zs_root, text);

	let output = output.into_inner();

	for file in output {
		let file_id = project.paths_mut().get_or_intern_nocase(&file.path);
		project.set_file(file_id, file.source);
		project.set_dirty(file_id);
	}

	Ok(diags.into_inner())
}

pub(crate) fn full_reparse(sfile: &mut SourceFile) -> UnitResult {
	let (green, errors) = doomfront::parse(
		&sfile.text,
		doomfront::zdoom::zscript::parse::file,
		doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
	)
	.into_inner();

	sfile.parsed = Some(ParsedFile {
		green,
		symbols: vec![],
		errors: ParseErrors::ZScript(errors),
	});

	Ok(())
}

#[allow(unused)]
pub(crate) fn partial_reparse(
	sfile: &mut SourceFile,
	deltas: impl Iterator<Item = TextDelta>,
) -> UnitResult {
	for delta in deltas {
		let start_lc = LineCol {
			line: delta.range.start.line,
			col: delta.range.start.character,
		};

		let end_lc = LineCol {
			line: delta.range.end.line,
			col: delta.range.end.character,
		};

		let start = sfile.lndx.offset(start_lc).ok_or(Error::Process {
			source: None,
			ctx: format!("invalid position {start_lc:#?}"),
		})?;
		let end = sfile.lndx.offset(end_lc).ok_or({
			Error::Process {
				source: None,
				ctx: format!("invalid position {end_lc:#?}"),
			}
		})?;

		splice(
			sfile,
			(
				TextRange::new(start, end),
				TextSize::from(delta.new_text_len as u32),
			),
		);
	}

	Ok(())
}

/// `text` must be the file's entire content.
/// `green` must be tagged [`Syn::Root`]; the returned node is tagged as such too.
fn splice(sfile: &mut SourceFile, changed: (TextRange, TextSize)) {
	use doomfront::zdoom::zscript::parse;

	let Some(parsed) = sfile.parsed.as_mut() else { unreachable!() };
	let ParseErrors::ZScript(errors) = &mut parsed.errors;
	let cursor = SyntaxNode::new_root(parsed.green.clone());

	let to_reparse = cursor
		.children()
		.find(|node| node.text_range().contains_range(changed.0));

	let Some(to_reparse) = to_reparse else {
		let (green, errs) =

		doomfront::parse(
			&sfile.text,
			parse::file,
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		).into_inner();

		*errors = errs;
		parsed.green = green;
		return;
	};

	let parser = match to_reparse.kind() {
		Syn::Error => {
			let (green, errs) = doomfront::parse(
				&sfile.text,
				parse::file,
				doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
			)
			.into_inner();

			*errors = errs;
			parsed.green = green;
			return;
		}
		Syn::ClassDef => parse::class_def,
		Syn::MixinClassDef => parse::mixin_class_def,
		Syn::StructDef => parse::struct_def,
		Syn::ClassExtend | Syn::StructExtend => parse::class_or_struct_extend,
		Syn::EnumDef => parse::enum_def,
		Syn::IncludeDirective => parse::include_directive,
		Syn::VersionDirective => parse::version_directive,
		Syn::ConstDef => parse::const_def,
		_ => unreachable!(),
	};

	let old_range = to_reparse.text_range();

	let new_range = if changed.1 > changed.0.len() {
		TextRange::new(old_range.start(), old_range.end() + changed.1)
	} else {
		TextRange::new(old_range.start(), old_range.end() - changed.0.len())
	};

	errors.retain(|error| !new_range.contains_range(error.found().text_range()));

	let (new_child, mut errs) = doomfront::parse(
		&sfile.text[new_range],
		parser,
		doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
	)
	.into_inner();

	errors.append(&mut errs);

	parsed.green = parsed
		.green
		.replace_child(to_reparse.index(), NodeOrToken::Node(new_child));

	debug_assert_eq!(parsed.green.kind(), Syn::kind_to_raw(Syn::Root));
}

#[must_use]
pub(crate) fn parse_errors_to_diags(
	sfile: &SourceFile,
	errors: &Vec<ParseError<Syn>>,
) -> Vec<Diagnostic> {
	let mut ret = Vec::with_capacity(errors.len());

	for error in errors {
		let offs_range = error.found().text_range();
		let start_lc = sfile.lndx.line_col(offs_range.start());
		let end_lc = sfile.lndx.line_col(offs_range.end());

		ret.push(Diagnostic {
			range: lsp_types::Range {
				start: Position {
					line: start_lc.line,
					character: start_lc.col,
				},
				end: Position {
					line: end_lc.line,
					character: end_lc.col,
				},
			},
			severity: Some(DiagnosticSeverity::ERROR),
			code: None,
			code_description: None,
			source: Some("doomls-parser".to_string()),
			message: {
				let mut msg = "Expected one of the following:".to_string();

				for expected in error.expected() {
					msg.push_str("\r\n");
					msg.push_str("- ");
					msg.push_str(expected);
				}

				msg
			},
			related_information: None,
			tags: None,
			data: None,
		});
	}

	ret
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn smoke_partial_reparse() {
		const SOURCE: &str = r#"/// A mixin class that does something.
mixin class doomls_Pickup
{
	Default
	{
		+FLAGSET
	}
\
}"#;

		let (green, errors) = doomfront::parse(
			SOURCE,
			doomfront::zdoom::zscript::parse::file,
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		)
		.into_inner();

		let mut sfile = SourceFile {
			lang: LangId::ZScript,
			text: SOURCE.to_string(),
			lndx: LineIndex::new(SOURCE),
			parsed: Some(ParsedFile {
				green,
				symbols: vec![],
				errors: ParseErrors::ZScript(errors),
			}),
		};

		let removed = sfile.text.remove(93);
		assert_eq!(removed, '\\');
		sfile.lndx = LineIndex::new(&sfile.text);

		let result = partial_reparse(
			&mut sfile,
			[TextDelta {
				range: lsp_types::Range {
					start: Position {
						line: 7,
						character: 0,
					},
					end: Position {
						line: 7,
						character: 1,
					},
				},
				new_text_len: 0,
			}]
			.into_iter(),
		);

		assert!(result.is_ok());
	}

	#[test]
	fn partial_reparse_errors() {
		const SOURCE: &str = r#"/// A mixin class that does something.
mixin class doomls_Pickup
{
	Default
	{
		+FLAGSET
	}

	meta Actor a
}"#;

		let (green, errors) = doomfront::parse(
			SOURCE,
			doomfront::zdoom::zscript::parse::file,
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		)
		.into_inner();

		let mut sfile = SourceFile {
			lang: LangId::ZScript,
			text: SOURCE.to_string(),
			lndx: LineIndex::new(SOURCE),
			parsed: Some(ParsedFile {
				green,
				symbols: vec![],
				errors: ParseErrors::ZScript(errors),
			}),
		};

		assert_eq!(sfile.parse_diagnostics().len(), 1);
		sfile.text.insert(107, ';');
		sfile.lndx = LineIndex::new(&sfile.text);

		let result = partial_reparse(
			&mut sfile,
			[TextDelta {
				range: lsp_types::Range {
					start: Position {
						line: 8,
						character: 12,
					},
					end: Position {
						line: 8,
						character: 12,
					},
				},
				new_text_len: 1,
			}]
			.into_iter(),
		);

		assert!(result.is_ok());
		assert!(sfile.parse_diagnostics().is_empty());
	}
}
