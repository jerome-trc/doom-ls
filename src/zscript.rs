mod data;
mod highlight;

use std::{
	ops::ControlFlow,
	path::{Path, PathBuf},
	sync::Arc,
};

use doomfront::{
	logos::Source,
	rowan::{ast::AstNode, GreenNode, NodeOrToken, TextRange, TextSize},
	zdoom::zscript::{ast, Syn, SyntaxNode},
};
use lsp_server::{Connection, Message, RequestId, Response};
use lsp_types::{
	Diagnostic, DiagnosticSeverity, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
	LanguageString, MarkedString, Position, SemanticTokens, SemanticTokensRangeResult,
	SemanticTokensResult,
};
use parking_lot::Mutex;
use rayon::prelude::*;
use tracing::warn;

use crate::{
	lines::{LineCol, LineIndex, TextDelta},
	project::{self, FileId, FilePos, ParsedFile, Project, QName, SourceFile},
	semtokens::Highlighter,
	util,
	zpath::{ZPath, ZPathBuf},
	Core, ErrorBox, FxIndexSet, LangId, MsgError, UnitResult,
};

pub(crate) use self::data::*;

// Request handling ////////////////////////////////////////////////////////////

pub(super) fn req_semtokens_range(
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

// Notification handling ///////////////////////////////////////////////////////

/// Uses the [`rayon`] global thread pool.
/// In the `Err` case, the root itself could not be read.
/// `root` will always have the file stem `ZSCRIPT` (any ASCII casing),
/// with one or no extension.
/// Elements in `zpaths` are absolute (from LSP `file` URIs).
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
		let green = doomfront::parse(
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
				.get_fileid_z(ZPath::new(&complete))
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
					dirty: true,
					symbols: vec![],
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
	}

	Ok(diags.into_inner())
}

pub(crate) fn full_reparse(sfile: &mut SourceFile) -> UnitResult {
	let green = doomfront::parse(
		&sfile.text,
		doomfront::zdoom::zscript::parse::file,
		doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
	)
	.into_inner();

	sfile.parsed = Some(ParsedFile {
		green,
		dirty: true,
		symbols: vec![],
	});

	Ok(())
}

pub(crate) fn partial_reparse(
	sfile: &mut SourceFile,
	deltas: impl Iterator<Item = TextDelta>,
) -> UnitResult {
	let Some(parsed) = sfile.parsed.as_mut() else { unreachable!() };

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

		parsed.green = splice(
			&sfile.text,
			parsed.green.clone(),
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
#[must_use]
fn splice(text: &str, green: GreenNode, changed: (TextRange, TextSize)) -> GreenNode {
	use doomfront::zdoom::zscript::parse;

	let cursor = SyntaxNode::new_root(green.clone());

	let to_reparse = cursor
		.children()
		.find(|node| node.text_range().contains_range(changed.0));

	let Some(to_reparse) = to_reparse else {
		return doomfront::parse(
			text,
			parse::file,
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		).into_inner();
	};

	let parser = match to_reparse.kind() {
		Syn::Error => {
			return doomfront::parse(
				text,
				parse::file,
				doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
			)
			.into_inner();
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
	let new_end = old_range.end().min(changed.0.start() + changed.1);

	let new_child = doomfront::parse(
		&text[TextRange::new(old_range.start(), new_end + TextSize::from(1))],
		parser,
		doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
	)
	.into_inner();

	green.replace_child(to_reparse.index(), NodeOrToken::Node(new_child))
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn smoke_splicing_reparse() {
		const SOURCE: &str = r#"

/// This class undoubtedly does something remarkable.
class Something : SomethingElse {
	private uint a_field;
}

"#;

		let green = doomfront::parse(
			SOURCE,
			doomfront::zdoom::zscript::parse::file,
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		)
		.into_inner();

		const SOURCE_CHANGED: &str = r#"

/// This class undoubtedly does something remarkable.
class Something : SomethingElse {}

"#;

		let mut source = SOURCE.to_string();
		source.replace_range(89..113, "");
		assert_eq!(source, SOURCE_CHANGED);

		let _ = splice(
			&source,
			green,
			(
				TextRange::new(TextSize::from(89), TextSize::from(113)),
				TextSize::from(0),
			),
		);
	}
}
