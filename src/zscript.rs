mod data;

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
	zpath::ZPathBuf,
	Core, ErrorBox, FxIndexSet, LangId, MsgError, UnitResult,
};

pub(crate) use self::data::*;

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
