//! Handling for `textDocument/semanticTokens/full` and
//! `textDocument/semanticTokens/range` requests.

use std::sync::OnceLock;

use doomfront::{
	rowan::{ast::AstNode, TextRange, TextSize, WalkEvent},
	zdoom::zscript::{ast, Syntax, SyntaxElem, SyntaxNode, SyntaxToken},
};
use lsp_server::{ErrorCode, Message, Response};
use lsp_types::{SemanticToken, SemanticTokens, SemanticTokensRangeResult, SemanticTokensResult};
use regex::Regex;
use zscript::sema;

use crate::{
	data::{Definition, FileSpan, SymGraphKey, SymGraphVal, Symbol},
	error::Error,
	langs::zscript,
	lines::LineCol,
	request,
	semtok::{Highlighter, SemToken, SemTokenFlags},
	UnitResult,
};

use super::sema::{FieldFlags, FunctionFlags};

pub(crate) fn full(ctx: request::Context) -> UnitResult {
	let green = ctx.src.green.as_ref().unwrap();
	let cursor = SyntaxNode::new_root(green.clone());
	let highlights = walk(&ctx, cursor);

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

pub(crate) fn range(ctx: request::Context, range: lsp_types::Range) -> UnitResult {
	let lc_start = LineCol {
		line: range.start.line,
		col: range.end.character,
	};

	let lc_end = LineCol {
		line: range.end.line,
		col: range.end.character,
	};

	let Some(start) = ctx.src.lines.offset(lc_start) else {
		return Err(Error::Process {
			source: None,
			ctx: format!("invalid position {lc_start:#?}"),
		}
		.map_to_response(ctx.id, ErrorCode::InvalidParams));
	};

	let Some(end) = ctx.src.lines.offset(lc_end) else {
		return Err(Error::Process {
			source: None,
			ctx: format!("invalid position {lc_end:#?}"),
		}
		.map_to_response(ctx.id, ErrorCode::InvalidParams));
	};

	let green = ctx.src.green.as_ref().unwrap();
	let file_node = SyntaxNode::new_root(green.clone());

	let node = match file_node.covering_element(TextRange::new(start, end)) {
		SyntaxElem::Node(node) => node,
		SyntaxElem::Token(token) => token.parent().unwrap(),
	};

	let highlights = walk(&ctx, node);

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
fn walk(ctx: &request::Context, cursor: SyntaxNode) -> Vec<SemanticToken> {
	let mut highlighter = Highlighter::new(&ctx.src.lines);

	for event in cursor.preorder_with_tokens() {
		let WalkEvent::Enter(enter) = event else {
			continue;
		};

		let SyntaxElem::Token(token) = enter else {
			continue;
		};

		if token.kind() != Syntax::Ident {
			highlight_non_ident(ctx, &mut highlighter, token);
		} else {
			highlight_ident(ctx, &mut highlighter, token);
		}
	}

	highlighter.finish()
}

fn highlight_ident(ctx: &request::Context, hl: &mut Highlighter, token: SyntaxToken) {
	let fspan = FileSpan {
		file_id: ctx.src.id,
		span: token.text_range(),
	};

	let Some(SymGraphVal::Symbol(sym_ptr)) =
		ctx.core.ready.sym_graph.get(&SymGraphKey::Reference(fspan))
	else {
		return;
	};

	match sym_ptr.as_ref().unwrap() {
		Symbol::User(u_sym) => {
			if let Some(def) = u_sym.definition() {
				highlight_by_definition(hl, token, def);
			}
		}
		Symbol::Internal(in_sym) => {
			highlight_by_definition(hl, token, &in_sym.def);
		}
	}
}

fn highlight_by_definition(hl: &mut Highlighter, token: SyntaxToken, def: &Definition) {
	match def {
		Definition::ZScript(sema::Datum::Class(_)) => {
			hl.advance(SemToken::Class, token.text_range());
		}
		Definition::ZScript(sema::Datum::Constant) => {
			hl.advance(SemToken::Constant, token.text_range());
		}
		Definition::ZScript(sema::Datum::Enum(_)) => {
			hl.advance(SemToken::Enum, token.text_range());
		}
		Definition::ZScript(sema::Datum::Field(sema::Field {
			flags, deprecated, ..
		})) => {
			hl.advance_mod(SemToken::Property, token.text_range(), {
				let mut stf = SemTokenFlags::MEMBER;

				if flags.contains(FieldFlags::READONLY) {
					stf.insert(SemTokenFlags::READONLY);
				}

				if deprecated.is_some() {
					stf.insert(SemTokenFlags::DEPRECATED);
				}

				stf
			});
		}
		Definition::ZScript(sema::Datum::Function(sema::Function {
			flags, deprecated, ..
		})) => {
			hl.advance_mod(
				if flags.contains(FunctionFlags::STATIC) {
					SemToken::Function
				} else {
					SemToken::Method
				},
				token.text_range(),
				{
					let mut stf = SemTokenFlags::empty();

					if !flags.contains(FunctionFlags::STATIC) {
						stf.insert(SemTokenFlags::MEMBER);
					}

					if deprecated.is_some() {
						stf.insert(SemTokenFlags::DEPRECATED);
					}

					stf
				},
			);
		}
		Definition::ZScript(sema::Datum::_MixinClass) => {
			hl.advance(SemToken::Macro, token.text_range());
		}
		Definition::ZScript(sema::Datum::Primitive(_)) => {
			hl.advance(SemToken::Keyword, token.text_range());
		}
		Definition::ZScript(sema::Datum::Struct(_)) => {
			hl.advance(SemToken::Struct, token.text_range());
		}
		Definition::_CVarInfo(_) => {} // TODO
	}
}

fn highlight_non_ident(ctx: &request::Context, hl: &mut Highlighter, token: SyntaxToken) {
	let syn = token.kind();
	let range = token.text_range();

	if syn.is_keyword() {
		match syn {
			Syntax::KwBreak
			| Syntax::KwCase
			| Syntax::KwContinue
			| Syntax::KwDo
			| Syntax::KwElse
			| Syntax::KwFail
			| Syntax::KwForEach
			| Syntax::KwFor
			| Syntax::KwGoto
			| Syntax::KwInclude
			| Syntax::KwIf
			| Syntax::KwLoop
			| Syntax::KwReturn
			| Syntax::KwStop
			| Syntax::KwSwitch
			| Syntax::KwUntil
			| Syntax::KwVersion
			| Syntax::KwWait
			| Syntax::KwWhile => hl.advance_mod(SemToken::Keyword, range, SemTokenFlags::CONTROL_FLOW),
			Syntax::KwAbstract
			| Syntax::KwAction
			| Syntax::KwArray
			| Syntax::KwBool
			| Syntax::KwBright
			| Syntax::KwByte
			| Syntax::KwCanRaise
			| Syntax::KwChar
			| Syntax::KwClass
			| Syntax::KwClearScope
			| Syntax::KwColor
			| Syntax::KwConst
			| Syntax::KwDefault
			| Syntax::KwDeprecated
			| Syntax::KwDouble
			| Syntax::KwEnum
			| Syntax::KwExtend
			| Syntax::KwFalse
			| Syntax::KwFast
			| Syntax::KwFinal
			| Syntax::KwFlagDef
			| Syntax::KwFloat
			| Syntax::KwInt
			| Syntax::KwInt16
			| Syntax::KwInt8
			| Syntax::KwInternal
			| Syntax::KwIn
			| Syntax::KwLatent
			| Syntax::KwLet
			| Syntax::KwLight
			| Syntax::KwLong
			| Syntax::KwMap
			| Syntax::KwMapIterator
			| Syntax::KwMeta
			| Syntax::KwMixin
			| Syntax::KwName
			| Syntax::KwNative
			| Syntax::KwNoDelay
			| Syntax::KwNone
			| Syntax::KwOffset
			| Syntax::KwOut
			| Syntax::KwOverride
			| Syntax::KwPlay
			| Syntax::KwPrivate
			| Syntax::KwProperty
			| Syntax::KwProtected
			| Syntax::KwReadOnly
			| Syntax::KwSByte
			| Syntax::KwShort
			| Syntax::KwSlow
			| Syntax::KwSound
			| Syntax::KwState
			| Syntax::KwStates
			| Syntax::KwStatic
			| Syntax::KwString
			| Syntax::KwStruct
			| Syntax::KwSuper
			| Syntax::KwReplaces
			| Syntax::KwTransient
			| Syntax::KwTrue
			| Syntax::KwUi
			| Syntax::KwUInt
			| Syntax::KwUInt16
			| Syntax::KwUInt8
			| Syntax::KwULong
			| Syntax::KwUShort
			| Syntax::KwVar
			| Syntax::KwVarArg
			| Syntax::KwVector2
			| Syntax::KwVector3
			| Syntax::KwVirtual
			| Syntax::KwVirtualScope
			| Syntax::KwVoid
			| Syntax::KwAuto
			| Syntax::KwVolatile => hl.advance(SemToken::Keyword, range),
			Syntax::KwAlignOf | Syntax::KwCross | Syntax::KwDot | Syntax::KwIs | Syntax::KwSizeOf => {
				hl.advance(SemToken::Operator, range)
			}
			_ => {}
		};

		return;
	}

	if syn.is_glyph() {
		if let Some(parent) = token.parent() {
			match parent.kind() {
				Syntax::BinExpr => {
					let e_bin = ast::BinExpr::cast(parent).unwrap();

					if e_bin.operator().0 == token {
						hl.advance(SemToken::Operator, range);
					}
				}
				Syntax::PrefixExpr => {
					let e_pre = ast::PrefixExpr::cast(parent).unwrap();

					if e_pre.operator().0 == token {
						hl.advance(SemToken::Operator, range);
					}
				}
				Syntax::PostfixExpr => {
					let e_post = ast::PostfixExpr::cast(parent).unwrap();

					if e_post.operator().0 == token {
						hl.advance(SemToken::Operator, range);
					}
				}
				Syntax::TernaryExpr => {
					let e_tern = ast::TernaryExpr::cast(parent).unwrap();

					if e_tern.question_mark() == token
						|| e_tern.colon().is_ok_and(|colon| colon == token)
					{
						hl.advance(SemToken::Operator, range);
					}
				}
				_ => {}
			}
		}

		return;
	}

	match syn {
		Syntax::IntLit | Syntax::FloatLit => hl.advance(SemToken::Number, range),
		Syntax::StringLit => highlight_string_literal(ctx, hl, token),
		Syntax::NameLit => {
			// TODO: context sensitivity.
			hl.advance(
				SemToken::String,
				TextRange::new(range.start(), range.start() + TextSize::from(1)),
			);
			hl.advance(
				SemToken::TypeParam,
				TextRange::new(
					range.start() + TextSize::from(1),
					range.end() - TextSize::from(1),
				),
			);
			hl.advance(
				SemToken::String,
				TextRange::new(range.end() - TextSize::from(1), range.end()),
			);
		}
		Syntax::NullLit => hl.advance(SemToken::Keyword, range),
		Syntax::RegionStart | Syntax::RegionEnd => {
			hl.advance_mod(SemToken::Keyword, range, SemTokenFlags::CONTROL_FLOW)
		}
		// TODO: highlighting for zscdoc links.
		Syntax::Comment | Syntax::DocComment => hl.advance(SemToken::Comment, range),
		// TODO: state sprites and frames...?
		_ => {} // Whitespace, unknown, or previously handled.
	}
}

fn highlight_string_literal(_: &request::Context, hl: &mut Highlighter, token: SyntaxToken) {
	if let Some(parent) = token.parent() {
		match parent.kind() {
			Syntax::IncludeDirective => {
				hl.advance_mod(SemToken::String, token.text_range(), SemTokenFlags::LINK);
				return;
			}
			Syntax::DeprecationQual | Syntax::StateLight | Syntax::VersionQual | Syntax::VersionDirective => {
				hl.advance(SemToken::String, token.text_range());
				return;
			}
			_ => {}
		}
	}

	static REGEX: OnceLock<Regex> = OnceLock::new();

	let rgx = REGEX.get_or_init(|| {
		// Row 0 specifies whitespace insignificance;
		// row 1 is for formatting specifiers;
		// row 2 is for LANGUAGE IDs;
		// row 3 is for short-form colour escape sequences;
		// row 4 is for long-form colour escape sequences;
		// row 5 is for C-like escape sequences.
		Regex::new(
			r#"(?x)
			(%[spcdiuxXofFeEgGaA])|
			(\$\$?[A-Za-z0-9_]+)|
			(\\c(?:\w|-))|
			(\\c(?:\[[_\d\w]+\]|-))|
			(\\[abfnrtv?"]|\\[0-7]{3}|\\x[A-Fa-f0-9]{2})
			"#,
		)
		.unwrap()
	});

	let mut pos = token.text_range().start();
	let mut any = false;

	for capset in rgx.captures_iter(token.text()) {
		let (semtoken, range) = if let Some(capture) = capset.get(1) {
			(SemToken::FormatSpec, capture.range())
		} else if let Some(capture) = capset.get(2) {
			(SemToken::Constant, capture.range()) // LANGUAGE string ID
		} else if let Some(capture) = capset.get(3).or(capset.get(4)).or(capset.get(5)) {
			(SemToken::EscapeSeq, capture.range())
		} else {
			unreachable!()
		};

		let r = TextRange::new(
			TextSize::from(u32::from(token.text_range().start()) + range.start as u32),
			TextSize::from(u32::from(token.text_range().start()) + range.end as u32),
		);

		let s = TextRange::new(pos, r.start());

		if s.len() > TextSize::from(0) {
			hl.advance(SemToken::String, s);
		}

		hl.advance(semtoken, r);
		pos = r.end();
		any = true;
	}

	if !any {
		hl.advance(SemToken::String, token.text_range());
	} else {
		hl.advance(
			SemToken::String,
			TextRange::new(pos, token.text_range().end()),
		);
	}
}
