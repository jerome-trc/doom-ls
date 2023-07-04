//! Semantic token highlighting.

use doomfront::{
	rowan::WalkEvent,
	zdoom::zscript::{Syn, SyntaxElem, SyntaxNode, SyntaxToken},
};

use crate::semtokens::{Highlighter, SemToken};

pub(crate) struct Context {
	pub(crate) hl: Highlighter,
	// TODO: Some kind of high-level semantic representation here.
}

pub(super) fn traverse(ctx: &mut Context, cursor: SyntaxNode) {
	debug_assert_eq!(cursor.kind(), Syn::Root);

	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	enum IdentMode {
		Constant,
		/// Only for actor state usage arguments.
		Control,
		Function,
		Super,
		Type,
		Var,
	}

	let mut ident_mode = vec![];
	let mut operators = 0;

	for event in cursor.preorder_with_tokens() {
		match event {
			WalkEvent::Enter(enter) => match enter {
				SyntaxElem::Token(token) => {
					let range = token.text_range();

					if token.kind() != Syn::Ident {
						highlight_token(ctx, &token, operators > 0);
					} else {
						// No highlighting at the root, since no identifiers are expected.
						let Some(idmode) = ident_mode.last() else { continue; };

						match idmode {
							IdentMode::Function => {
								ctx.hl.advance(SemToken::Function, range);
							}
							IdentMode::Type => {
								ctx.hl.advance(SemToken::Type, range);
							}
							IdentMode::Var => {
								// TODO: Semantically-aware identifier highlighting.
								if !token.text().eq_ignore_ascii_case("self") {
									ctx.hl.advance(SemToken::Variable, range);
								} else {
									ctx.hl.advance(SemToken::Modifier, range);
								}
							}
							IdentMode::Super => {
								ctx.hl.advance(SemToken::Modifier, range);
							}
							IdentMode::Control => {
								ctx.hl.advance(SemToken::Keyword, range);
							}
							IdentMode::Constant => {
								ctx.hl.advance(SemToken::Constant, range);
							}
						}
					}
				}
				SyntaxElem::Node(node) => match node.kind() {
					Syn::EnumVariant | Syn::ConstDef => ident_mode.push(IdentMode::Constant),
					Syn::TypeRef
					| Syn::ClassDef
					| Syn::ClassExtend
					| Syn::EnumDef
					| Syn::StructDef
					| Syn::StructExtend => ident_mode.push(IdentMode::Type),
					Syn::VarName
					| Syn::ArgList
					| Syn::FieldDecl
					| Syn::FlagDef
					| Syn::ParamList
					| Syn::PropertyDef
					| Syn::PropertySetting
					| Syn::MemberExpr
					| Syn::IdentExpr => {
						ident_mode.push(IdentMode::Var);
					}
					Syn::FunctionDecl | Syn::ActionFunction | Syn::CallExpr => {
						ident_mode.push(IdentMode::Function);
					}
					Syn::BinExpr | Syn::PostfixExpr | Syn::PrefixExpr => {
						operators += 1;
					}
					Syn::SuperExpr => {
						ident_mode.push(IdentMode::Super);
					}
					Syn::StatesUsage => {
						ident_mode.push(IdentMode::Control);
					}
					_ => {}
				},
			},
			WalkEvent::Leave(leave) => match leave {
				SyntaxElem::Node(node) => match node.kind() {
					Syn::EnumVariant | Syn::ConstDef => {
						let _ = ident_mode.pop();
					}
					Syn::TypeRef
					| Syn::ClassDef
					| Syn::ClassExtend
					| Syn::EnumDef
					| Syn::StructDef
					| Syn::StructExtend => {
						let _ = ident_mode.pop();
					}
					Syn::VarName
					| Syn::ArgList
					| Syn::FieldDecl
					| Syn::FlagDef
					| Syn::ParamList
					| Syn::PropertyDef
					| Syn::PropertySetting
					| Syn::MemberExpr
					| Syn::IdentExpr => {
						let _ = ident_mode.pop();
					}
					Syn::FunctionDecl | Syn::ActionFunction | Syn::CallExpr => {
						let _ = ident_mode.pop();
					}
					Syn::BinExpr | Syn::PostfixExpr | Syn::PrefixExpr => {
						operators -= 1;
					}
					Syn::SuperExpr => {
						let _ = ident_mode.pop();
					}
					Syn::StatesUsage => {
						let _ = ident_mode.pop();
					}
					_ => {}
				},
				SyntaxElem::Token(_) => {}
			},
		}
	}
}

// Common //////////////////////////////////////////////////////////////////////

/// Note that this does not highlight identifiers, which need special handling.
fn highlight_token(ctx: &mut Context, token: &SyntaxToken, operator: bool) {
	let syn = token.kind();
	let range = token.text_range();

	if syn.is_keyword() {
		match syn {
			Syn::KwBreak
			| Syn::KwCase
			| Syn::KwContinue
			| Syn::KwDo
			| Syn::KwElse
			| Syn::KwFail
			| Syn::KwForEach
			| Syn::KwFor
			| Syn::KwGoto
			| Syn::KwIf
			| Syn::KwLoop
			| Syn::KwReturn
			| Syn::KwStop
			| Syn::KwSwitch
			| Syn::KwUntil
			| Syn::KwVersion
			| Syn::KwWait
			| Syn::KwWhile => ctx.hl.advance(SemToken::Keyword, range),
			Syn::KwAbstract
			| Syn::KwAction
			| Syn::KwArray
			| Syn::KwBool
			| Syn::KwBright
			| Syn::KwByte
			| Syn::KwCanRaise
			| Syn::KwChar
			| Syn::KwClass
			| Syn::KwClearScope
			| Syn::KwColor
			| Syn::KwConst
			| Syn::KwDefault
			| Syn::KwDeprecated
			| Syn::KwDouble
			| Syn::KwEnum
			| Syn::KwExtend
			| Syn::KwFalse
			| Syn::KwFast
			| Syn::KwFinal
			| Syn::KwFlagDef
			| Syn::KwFloat
			| Syn::KwInt
			| Syn::KwInt16
			| Syn::KwInt8
			| Syn::KwInternal
			| Syn::KwIn
			| Syn::KwLatent
			| Syn::KwLet
			| Syn::KwLight
			| Syn::KwLong
			| Syn::KwMap
			| Syn::KwMapIterator
			| Syn::KwMeta
			| Syn::KwMixin
			| Syn::KwName
			| Syn::KwNative
			| Syn::KwNoDelay
			| Syn::KwNone
			| Syn::KwOffset
			| Syn::KwOut
			| Syn::KwOverride
			| Syn::KwPlay
			| Syn::KwPrivate
			| Syn::KwProperty
			| Syn::KwProtected
			| Syn::KwReadOnly
			| Syn::KwSByte
			| Syn::KwShort
			| Syn::KwSlow
			| Syn::KwSound
			| Syn::KwState
			| Syn::KwStates
			| Syn::KwStatic
			| Syn::KwString
			| Syn::KwStruct
			| Syn::KwSuper
			| Syn::KwReplaces
			| Syn::KwTransient
			| Syn::KwTrue
			| Syn::KwUi
			| Syn::KwUInt
			| Syn::KwUInt16
			| Syn::KwUInt8
			| Syn::KwULong
			| Syn::KwUShort
			| Syn::KwVar
			| Syn::KwVarArg
			| Syn::KwVector2
			| Syn::KwVector3
			| Syn::KwVirtual
			| Syn::KwVirtualScope
			| Syn::KwVoid
			| Syn::KwAuto
			| Syn::KwVolatile => ctx.hl.advance(SemToken::Modifier, range),
			Syn::KwAlignOf | Syn::KwCross | Syn::KwDot | Syn::KwIs | Syn::KwSizeOf => {
				ctx.hl.advance(SemToken::Operator, range)
			}
			_ => {}
		};

		return;
	}

	if syn.is_glyph() && operator {
		if matches!(
			syn,
			Syn::ParenL
				| Syn::ParenR | Syn::BracketL
				| Syn::BracketR | Syn::AngleL
				| Syn::AngleR | Syn::BraceL
				| Syn::BraceR
		) {
			return;
		}

		ctx.hl.advance(SemToken::Operator, range);
		return;
	}

	match syn {
		Syn::IntLit | Syn::FloatLit => ctx.hl.advance(SemToken::Number, range),
		Syn::StringLit | Syn::NameLit => ctx.hl.advance(SemToken::String, range),
		Syn::NullLit => ctx.hl.advance(SemToken::Modifier, range),
		Syn::PoundInclude | Syn::RegionStart | Syn::RegionEnd => {
			ctx.hl.advance(SemToken::Keyword, range)
		}
		Syn::Comment | Syn::DocComment => ctx.hl.advance(SemToken::Comment, range),
		// Whitespace, unknown, state sprites, state frames, or previously handled.
		_ => {}
	}
}

#[cfg(test)]
mod test {
	use doomfront::zdoom::zscript::ParseTree;

	use super::*;

	#[test]
	fn smoke() {
		const SOURCE: &str = r##""##; // TODO: need some test data...

		let newlines = crate::scan::compute_newlines(SOURCE).into();
		let mut ctx = Context {
			hl: Highlighter::new(newlines),
		};

		let ptree: ParseTree = doomfront::parse(
			SOURCE,
			doomfront::zdoom::zscript::parse::file,
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		);

		traverse(&mut ctx, ptree.cursor());
	}
}
