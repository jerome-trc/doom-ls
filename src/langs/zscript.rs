mod highlight;
mod sema;

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
use lsp_server::{Connection, Message, RequestId, Response};
use lsp_types::{
	Diagnostic, DiagnosticSeverity, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
	LanguageString, Location, MarkedString, Position, SemanticTokens, SemanticTokensRangeResult,
	SemanticTokensResult,
};
use parking_lot::Mutex;
use rayon::prelude::*;
use tracing::warn;

use crate::{
	lines::{LineCol, LineIndex, TextDelta},
	names::StringInterner,
	project::{self, FileId, FilePos, ParseErrors, ParsedFile, Project, SourceFile, SymbolDelta},
	semtokens::Highlighter,
	util,
	zpath::ZPath,
	Core, ErrorBox, LangId, MsgError, UnitResult,
};

pub(crate) use self::sema::*;

// Request handling ////////////////////////////////////////////////////////////

pub(crate) fn req_goto(
	core: &Core,
	conn: &Connection,
	sfile: &SourceFile,
	id: RequestId,
	position: Position,
	ix_project: usize,
) -> UnitResult {
	let parsed = sfile.parsed.as_ref().unwrap();
	let cursor = SyntaxNode::new_root(parsed.green.clone());

	let linecol = LineCol {
		line: position.line,
		col: position.character,
	};

	let Some(boffs) = sfile.lndx.offset(linecol) else {
		Core::respond_null(conn, id)?;

		return Err(
			Box::new(MsgError(
				format!("failed to get token at position {linecol:#?}")
			))
		);
	};

	let Some(token) = cursor.token_at_offset(boffs).next() else {
		Core::respond_null(conn, id)?;

		return Err(Box::new(MsgError(
			format!("failed to get token at position {linecol:#?}")
		)));
	};

	let handler = if token.kind() == Syn::Ident {
		let parent = token.parent().unwrap();

		if token.text().eq_ignore_ascii_case("self") && ast::IdentExpr::can_cast(parent.kind()) {
			// No useful information to provide here.
			Core::respond_null(conn, id)?;
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
		Core::respond_null(conn, id)?;
		tracing::debug!("GotoDefinition miss - unsupported token.");
		return Ok(());
	};

	// TODO: If the user put in a "go to definition" request on the identifier
	// making up the declaration, respond with a list of references.

	for i in (0..=ix_project).rev() {
		match handler(core, &core.projects[i], token.clone()) {
			ControlFlow::Continue(()) => {}
			ControlFlow::Break(Err(err)) => {
				return Err(err);
			}
			ControlFlow::Break(Ok(resp)) => {
				conn.sender.send(Message::Response(Response {
					id,
					result: Some(serde_json::to_value(resp).unwrap()),
					error: None,
				}))?;

				return Ok(());
			}
		}
	}

	tracing::debug!("GotoDefinition miss - unknown symbol.");
	Core::respond_null(conn, id)
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

	if let Some(project::SymbolKey::ZScript(sym_k)) =
		project.lookup_global(&core.strings.type_name_nocase(text))
	{
		return goto_symbol(project, sym_k, text);
	}

	if let Some(project::SymbolKey::ZScript(sym_k)) =
		project.lookup_global(&core.strings.var_name_nocase(text))
	{
		return goto_symbol(project, sym_k, text);
	}

	ControlFlow::Continue(())
}

fn goto_symbol(
	project: &Project,
	sym_k: SymbolKey,
	name: &str,
) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
	let storage = project.zscript();

	let filepos = match sym_k {
		SymbolKey::Class(class_k) => {
			let datum = &storage.data[class_k];
			let Datum::Class(class_d) = datum else { unreachable!() };
			class_d.filepos
		}
		SymbolKey::Constant(const_k) => {
			let datum = &storage.data[const_k];
			let Datum::Constant(const_d) = datum else { unreachable!() };
			const_d.filepos
		}
		SymbolKey::Enum(enum_k) => {
			let datum = &storage.data[enum_k];
			let Datum::Enum(enum_d) = datum else { unreachable!() };
			enum_d.filepos
		}
		SymbolKey::MixinClass(mixin_k) => {
			let datum = &storage.data[mixin_k];
			let Datum::MixinClass(mixin_d) = datum else { unreachable!() };
			mixin_d.filepos
		}
		SymbolKey::Struct(struct_k) => {
			let datum = &storage.data[struct_k];
			let Datum::Struct(struct_d) = datum else { unreachable!() };
			struct_d.filepos
		}
	};

	let path = project.get_path(filepos.file).unwrap();
	let sfile = project.get_file(filepos.file).unwrap();

	match util::make_location(&sfile.lndx, path, filepos.pos, name.len()) {
		Ok(l) => ControlFlow::Break(Ok(GotoDefinitionResponse::Scalar(l))),
		Err(err) => ControlFlow::Break(Err(Box::new(err))),
	}
}

pub(crate) fn req_hover(
	conn: &Connection,
	sfile: &SourceFile,
	id: RequestId,
	params: HoverParams,
) -> UnitResult {
	let Some(parsed) = &sfile.parsed else {
		return Core::respond_null(conn, id);
	};

	let pos = params.text_document_position_params.position;

	let Some(token) = parsed.token_at::<Syn>(pos, &sfile.lndx) else {
		warn!("Failed to find token specified by a hover request.");

		conn.sender.send(Message::Response(
			Response {
				id,
				result: None,
				error: None, // TODO
			}
		))?;

		return Ok(());
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
			conn.sender.send(Message::Response(
				Response {
					id,
					result: Some(serde_json::Value::Null),
					error: None,
				}
			))?;

			return Ok(());
		}
	};

	let resp = Response {
		id,
		result: Some(serde_json::to_value(Hover {
			contents,
			range: None,
		})?),
		error: None,
	};

	conn.sender.send(Message::Response(resp))?;
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
		Core::respond_null(conn, id)?;

		return Err(
			Box::new(MsgError(
				format!("failed to get token at position {linecol:#?}")
			))
		);
	};

	let Some(token) = cursor.token_at_offset(boffs).next() else {
		Core::respond_null(conn, id)?;

		return Err(Box::new(MsgError(
			format!("failed to get token at position {linecol:#?}")
		)));
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

pub(crate) fn req_semtokens_full(
	conn: &Connection,
	sfile: &SourceFile,
	id: RequestId,
) -> UnitResult {
	let Some(parsed) = &sfile.parsed else { unreachable!() };
	let cursor = SyntaxNode::new_root(parsed.green.clone());

	let resp = Response {
		id,
		result: Some(serde_json::to_value(SemanticTokensResult::Tokens(
			semtokens(cursor, &sfile.lndx),
		))?),
		error: None,
	};

	conn.sender.send(Message::Response(resp))?;
	Ok(())
}

pub(crate) fn req_semtokens_range(
	conn: &Connection,
	sfile: &SourceFile,
	id: RequestId,
	range: lsp_types::Range,
) -> UnitResult {
	let Some(parsed) = &sfile.parsed else { unreachable!() };
	let cursor = SyntaxNode::new_root(parsed.green.clone());

	let lc_start = LineCol {
		line: range.start.line,
		col: range.end.character,
	};

	let lc_end = LineCol {
		line: range.end.line,
		col: range.end.character,
	};

	let Some(start) = sfile.lndx.offset(lc_start) else {
		return Err(Box::new(MsgError(format!("invalid range start: {lc_start:?}"))));
	};

	let Some(end) = sfile.lndx.offset(lc_end) else {
		return Err(Box::new(MsgError(format!("invalid range end: {lc_end:?}"))));
	};

	let node = match cursor.covering_element(TextRange::new(start, end)) {
		NodeOrToken::Node(node) => node,
		NodeOrToken::Token(token) => token.parent().unwrap(),
	};

	let resp = Response {
		id,
		result: Some(serde_json::to_value(SemanticTokensRangeResult::Tokens(
			semtokens(node, &sfile.lndx),
		))?),
		error: None,
	};

	conn.sender.send(Message::Response(resp))?;
	Ok(())
}

#[must_use]
pub(self) fn semtokens(node: SyntaxNode, lndx: &LineIndex) -> SemanticTokens {
	let mut context = highlight::Context {
		hl: Highlighter::new(lndx),
	};

	highlight::traverse(&mut context, node);

	SemanticTokens {
		result_id: None,
		data: context.hl.tokens,
	}
}

pub(crate) fn semantic_update(
	strings: &StringInterner,
	storage: &mut Storage,
	delta: &mut SymbolDelta<SymbolKey>,
	file_id: FileId,
	green: GreenNode,
) {
	debug_assert_eq!(green.kind(), Syn::kind_to_raw(Syn::Root));
	let cursor = SyntaxNode::new_root(green);

	for removed in &delta.removed {
		storage.data.remove(removed.1.into_inner());
	}

	for child in cursor.children() {
		let Some(top) = ast::TopLevel::cast(child) else { continue; };

		match top {
			ast::TopLevel::ClassDef(classdef) => {
				let Ok(class_name) = classdef.name() else { continue; };

				let dat_k = storage.data.insert(Datum::Class(ClassDatum {
					filepos: FilePos {
						file: file_id,
						pos: class_name.text_range().start(),
					},
				}));

				let name = strings.type_name_nocase(class_name.text());
				delta.added.push((name, SymbolKey::Class(dat_k)));
			}
			ast::TopLevel::ConstDef(constdef) => {
				let Ok(const_name) = constdef.name() else { continue; };

				let dat_k = storage.data.insert(Datum::Constant(ConstantDatum {
					filepos: FilePos {
						file: file_id,
						pos: const_name.text_range().start(),
					},
				}));

				let name = strings.var_name_nocase(const_name.text());
				delta.added.push((name, SymbolKey::Constant(dat_k)));
			}
			ast::TopLevel::EnumDef(enumdef) => {
				let Ok(enum_name) = enumdef.name() else { continue; };

				let dat_k = storage.data.insert(Datum::Enum(EnumDatum {
					filepos: FilePos {
						file: file_id,
						pos: enum_name.text_range().start(),
					},
				}));

				let name = strings.type_name_nocase(enum_name.text());
				delta.added.push((name, SymbolKey::Enum(dat_k)));
			}
			ast::TopLevel::MixinClassDef(mixindef) => {
				let Ok(mixin_name) = mixindef.name() else { continue; };

				let dat_k = storage.data.insert(Datum::MixinClass(MixinClassDatum {
					filepos: FilePos {
						file: file_id,
						pos: mixin_name.text_range().start(),
					},
				}));

				let name = strings.type_name_nocase(mixin_name.text());
				delta.added.push((name, SymbolKey::MixinClass(dat_k)));
			}
			ast::TopLevel::StructDef(structdef) => {
				let Ok(struct_name) = structdef.name() else { continue; };

				let dat_k = storage.data.insert(Datum::Struct(StructDatum {
					filepos: FilePos {
						file: file_id,
						pos: struct_name.text_range().start(),
					},
				}));

				let name = strings.type_name_nocase(struct_name.text());
				delta.added.push((name, SymbolKey::Struct(dat_k)));
			}
			ast::TopLevel::ClassExtend(_)
			| ast::TopLevel::StructExtend(_)
			| ast::TopLevel::Include(_)
			| ast::TopLevel::Version(_) => continue,
		}
	}
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

		let start = sfile
			.lndx
			.offset(start_lc)
			.ok_or(MsgError("invalid range start".to_string()))?;
		let end = sfile
			.lndx
			.offset(end_lc)
			.ok_or(MsgError("invalid range end".to_string()))?;

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
