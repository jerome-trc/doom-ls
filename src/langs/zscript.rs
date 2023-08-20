//! Request and notification handling for ZDoom's [ZScript] language.
//!
//! [ZScript]: doomfront::zdoom::zscript

mod docsymbols;
mod goto;
pub(crate) mod highlight;
mod hover;
pub(crate) mod native;
pub(crate) mod sema;

use std::{
	ops::ControlFlow,
	path::{Path, PathBuf},
};

pub(crate) use doomfront::zdoom::zscript::Syn;
use doomfront::{
	rowan::{GreenNode, Language, NodeOrToken, TextRange, TextSize, WalkEvent},
	zdoom::zscript::{SyntaxNode, SyntaxToken},
	ParseError,
};
use lsp_server::{ErrorCode, Message, Response};
use lsp_types::{
	Diagnostic, DiagnosticSeverity, GotoDefinitionResponse, Location, Position, SemanticTokens,
	SemanticTokensRangeResult, SemanticTokensResult,
};
use parking_lot::Mutex;
use rayon::prelude::*;

use crate::{
	lines::{LineCol, LineIndex, TextDelta},
	names::IName,
	paths::PathInterner,
	project::{self, ParseErrors, ParsedFile, Project, SourceFile, StackedScope},
	request, util, Core, Error, ErrorBox, LangId, UnitResult,
};

pub(crate) use self::{docsymbols::req_doc_symbols, goto::req_goto, hover::req_hover, sema::*};

// Request handling ////////////////////////////////////////////////////////////

pub(crate) fn req_references(ctx: request::Context, pos: Position) -> UnitResult {
	let parsed = ctx.sfile.parsed.as_ref().unwrap();
	let cursor = SyntaxNode::new_root(parsed.green.clone());

	let linecol = LineCol {
		line: pos.line,
		col: pos.character,
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

	if token.kind() != Syn::Ident {
		// TODO:
		// - Core types (numeric primitives, array, map, et cetera).
		// - `Syn::NameLit`, often identifying a class.
		// - `Syn::StringLit`, which may be coerced to `name` and identify a class.
		// Also support LANGUAGE IDs, GLDEFS, maybe even CVar names.
		// - `Syn::KwSuper`; try to go to a parent class definition.
		return Core::respond_null(ctx.conn, ctx.id);
	}

	let subvecs = Mutex::new(vec![]);
	let ident = token.text();

	for i in (0..=ctx.ix_project).rev() {
		let project = &ctx.core.projects[i];
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

	ctx.conn.sender.send(Message::Response(Response {
		id: ctx.id,
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
	tracing::debug!("(Re)building ZScript include tree...");

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
