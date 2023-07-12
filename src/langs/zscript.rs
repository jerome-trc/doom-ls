//! Request and notification handling for ZDoom's [ZScript] language.
//!
//! [ZScript]: doomfront::zdoom::zscript

pub(crate) mod highlight;
pub(crate) mod sema;

use std::{
	ops::ControlFlow,
	path::{Path, PathBuf},
};

pub(crate) use doomfront::zdoom::zscript::Syn;
use doomfront::{
	rowan::{ast::AstNode, GreenNode, Language, NodeOrToken, TextRange, TextSize, WalkEvent},
	zdoom::zscript::{ast, SyntaxNode, SyntaxToken},
	ParseError,
};
use lsp_server::{Connection, ErrorCode, Message, RequestId, Response};
use lsp_types::{
	Diagnostic, DiagnosticSeverity, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
	LanguageString, Location, MarkedString, Position, SemanticTokens, SemanticTokensRangeResult,
	SemanticTokensResult,
};
use parking_lot::Mutex;
use rayon::prelude::*;

use crate::{
	lines::{LineCol, LineIndex, TextDelta},
	project::{self, ParseErrors, ParsedFile, Project, SourceFile},
	request, util,
	zpath::ZPath,
	Core, Error, ErrorBox, LangId, UnitResult,
};

pub(crate) use self::sema::*;

// Request handling ////////////////////////////////////////////////////////////

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

	let handler = if token.kind() == Syn::Ident {
		let parent = token.parent().unwrap();

		if token.text().eq_ignore_ascii_case("self") && ast::IdentExpr::can_cast(parent.kind()) {
			// No useful information to provide here.
			Core::respond_null(ctx.conn, ctx.id)?;
			tracing::debug!("GotoDefinition miss - `self` identifier.");
			return Ok(());
		}

		req_goto_ident
	// For now, only identifiable things can have "definitions".
	// TODO:
	// - `Syn::NameLit`, often identifying a class.
	// - `Syn::StringLit`, which may be coerced to `name` and identify a class.
	// Also support LANGUAGE IDs, GLDEFS, maybe even CVar names.
	// - `Syn::KwSuper`; try to go to a parent class definition.
	// - `Syn::DocComment`; support following intra-doc links.
	// - `Syn::KwString` / `Syn::KwArray` / `Syn::KwMap` / `Syn::KwMapIterator` /
	// `Syn::KwColor` / `Syn::KwVector2` / `Syn::KwVector3` by faking their definitions.
	} else {
		Core::respond_null(ctx.conn, ctx.id)?;
		tracing::debug!("GotoDefinition miss - unsupported token.");
		return Ok(());
	};

	// TODO: If the user put in a "go to definition" request on the identifier
	// making up the declaration, respond with a list of references.

	for project in ctx.core.projects_backwards_from(ctx.ix_project) {
		match handler(ctx.core, project, token.clone()) {
			ControlFlow::Continue(()) => {}
			ControlFlow::Break(Err(err)) => {
				return Err(Error::Process {
					source: Some(err),
					ctx: "go-to definition error".to_string(),
				});
			}
			ControlFlow::Break(Ok(resp)) => {
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
		}
	}

	tracing::debug!("GotoDefinition miss - unknown symbol.");
	Core::respond_null(ctx.conn, ctx.id)
}

/// Returns:
/// - `Continue` if the symbol hasn't been found.
/// - `Break(Ok)` if the symbol was resolved successfully.
fn req_goto_ident(
	core: &Core,
	project: &Project,
	token: SyntaxToken,
) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
	let text = token.text();

	// TODO: Rather than trying every namespace, narrow down based on context.

	if let Some(datum) = project.lookup_global(&core.strings.type_name_nocase(text)) {
		return goto_symbol(project, datum, text);
	}

	if let Some(datum) = project.lookup_global(&core.strings.var_name_nocase(text)) {
		return goto_symbol(project, datum, text);
	}

	ControlFlow::Continue(())
}

fn goto_symbol(
	project: &Project,
	datum: &project::Datum,
	name: &str,
) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
	let filepos = match datum {
		project::Datum::ZScript(dat_zs) => {
			if let Some(n) = dat_zs.name_filepos() {
				n
			} else {
				return ControlFlow::Break(Ok(GotoDefinitionResponse::Array(vec![])));
			}
		}
	};

	let path = project.get_path(filepos.file).unwrap();
	let sfile = project.get_file(filepos.file).unwrap();

	match util::make_location(&sfile.lndx, path, filepos.pos, name.len()) {
		Ok(l) => ControlFlow::Break(Ok(GotoDefinitionResponse::Scalar(l))),
		Err(err) => ControlFlow::Break(Err(Box::new(err))),
	}
}

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
			ctx.conn.sender.send(Message::Response(
				Response {
					id: ctx.id,
					result: Some(serde_json::Value::Null),
					error: None,
				}
			))?;

			return Ok(());
		}
	};

	let resp = Response {
		id: ctx.id,
		result: Some(
			serde_json::to_value(Hover {
				contents,
				range: None,
			})
			.unwrap(),
		),
		error: None,
	};

	ctx.conn.sender.send(Message::Response(resp))?;
	Ok(())
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

		project.all_files_par().for_each(|(file_id, sfile)| {
			let path = project.get_path(file_id).unwrap();
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
		project: &'p Project,
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
				.project
				.get_pathid_z(ZPath::new(&complete))
				 else {
					tracing::error!("Failed to get interned ZDoom path: `{}`", complete.display());
					return;
				};

			let real_path = ctx.project.get_path(file_id).unwrap();

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
		project,
		output: &output,
		diags: &diags,
	};

	recur(ctx, project.root(), zs_root, text);

	let output = output.into_inner();

	for file in output {
		let file_id = project.intern_pathbuf(file.path);
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
