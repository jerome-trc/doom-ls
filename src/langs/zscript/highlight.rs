//! Semantic token highlighting.

use doomfront::{
	rowan::{ast::AstNode, TextRange, TextSize, WalkEvent},
	zdoom::zscript::{ast, Syn, SyntaxElem, SyntaxNode, SyntaxToken},
};
use lsp_types::SemanticToken;

use crate::{
	langs::zscript::SymbolKey,
	project::{self, Project},
	semtokens::{Highlighter, SemToken, SemTokenFlags},
	Core,
};

pub(super) struct Context<'c> {
	pub(super) core: &'c Core,
	/// When performing global symbol table lookups, start here and work backwards.
	pub(super) ix_project: usize,
	pub(super) hl: Highlighter<'c>,
}

impl Context<'_> {
	#[must_use]
	pub(super) fn traverse(mut self, cursor: SyntaxNode) -> Vec<SemanticToken> {
		let start_time = std::time::Instant::now();

		for event in cursor.preorder_with_tokens() {
			match event {
				WalkEvent::Enter(enter) => match enter {
					SyntaxElem::Token(token) => {
						if token.kind() != Syn::Ident {
							self.highlight_non_ident(token);
						} else {
							self.highlight_ident(token);
						}
					}
					SyntaxElem::Node(_) => {}
				},
				WalkEvent::Leave(_) => {}
			}
		}

		tracing::debug!(
			"Semantic highlighting done in {}ms.",
			start_time.elapsed().as_millis()
		);

		self.hl.finish()
	}

	fn highlight_ident(&mut self, token: SyntaxToken) {
		debug_assert_eq!(token.kind(), Syn::Ident);
		let Some(parent) = token.parent() else { return; };
		let range = token.text_range();

		match parent.kind() {
			Syn::ActionFunction => {
				let action = ast::ActionFunction::cast(parent).unwrap();

				if action.into_call().is_some_and(|(t, _)| t == token) {
					self.hl.advance(SemToken::Method, range);
				}
			}
			Syn::Argument => {
				let arg = ast::Argument::cast(parent).unwrap();

				if arg.name().is_some_and(|n| n == token) {
					self.hl.advance(SemToken::Property, range);
				}
			}
			Syn::ClassCastExpr => {
				let e_cc = ast::ClassCastExpr::cast(parent).unwrap();

				if e_cc.class_name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Class, range);
				}
			}
			Syn::ClassDef => {
				let classdef = ast::ClassDef::cast(parent).unwrap();

				if classdef.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Class, range);
				}
			}
			Syn::ClassExtend => {
				let classext = ast::ClassExtend::cast(parent).unwrap();

				if classext.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Class, range);
				}
			}
			Syn::ConstDef => {
				let constdef = ast::ConstDef::cast(parent).unwrap();

				if constdef.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Constant, range);
				}
			}
			Syn::DeclAssignStat => {
				let s_da = ast::DeclAssignStat::cast(parent).unwrap();

				if s_da.idents().any(|i| i == token) {
					self.hl.advance(SemToken::Property, range);
				}
			}
			Syn::EnumDef => {
				let enumdef = ast::EnumDef::cast(parent).unwrap();

				if enumdef.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Enum, range);
				}
			}
			Syn::EnumVariant => {
				let variant = ast::EnumVariant::cast(parent).unwrap();

				if variant.name() == token {
					self.hl.advance(SemToken::Constant, range);
				}
			}
			Syn::FlagDef | Syn::FieldDecl | Syn::PropertyDef => {
				self.hl.advance(SemToken::Property, range);
			}
			Syn::FunctionDecl => {
				self.hl.advance(SemToken::Function, range);
			}
			Syn::IdentChain => {
				let grandparent = parent.parent().unwrap();

				match grandparent.kind() {
					Syn::IdentChainType => {
						self.highlight_type_name(token);
					}
					Syn::StateFlow | Syn::StateLabel => {
						if parent.last_token().is_some_and(|t| t == token) {
							self.hl.advance(SemToken::Property, range);
						} else {
							self.hl.advance(SemToken::Namespace, range);
						}
					}
					// TODO: Semantic information needed to specify.
					_ => {}
				}
			}
			Syn::IdentExpr => {
				const KWS: &[&str] = &["self", "invoker", "default"];
				let grandparent = parent.parent().unwrap();

				if grandparent.kind() == Syn::CallExpr {
					self.hl.advance(SemToken::Function, range);
				} else if KWS.iter().any(|kw| kw.eq_ignore_ascii_case(token.text())) {
					self.hl.advance(SemToken::Keyword, range);
				} else {
					// TODO: Stack of variable scopes.
					self.hl.advance(SemToken::Property, range);
				}
			}
			Syn::InheritSpec => {
				let grandparent = parent.parent().unwrap();
				let classdef = ast::ClassDef::cast(grandparent).unwrap();
				let ancestor = classdef.parent_class().unwrap();

				if ancestor == token {
					self.hl.advance(SemToken::Class, range);
				}
			}
			Syn::LocalVarInit => {
				let lvi = ast::LocalVarInit::cast(parent).unwrap();

				if lvi.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Property, range);
				}
			}
			Syn::MemberExpr => {
				let grandparent = parent.parent().unwrap();
				let e_mem = ast::MemberExpr::cast(parent).unwrap();
				// TODO: Disambiguate member variables from locals and globals.

				if e_mem.member_name().is_ok_and(|n| n == token) {
					if grandparent.kind() == Syn::CallExpr {
						self.hl.advance(SemToken::Function, range);
					} else {
						// TODO: Stack of variable scopes.
						self.hl.advance(SemToken::Property, range);
					}
				}
			}
			Syn::MixinClassDef => {
				let mixindef = ast::MixinClassDef::cast(parent).unwrap();

				if mixindef.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Macro, range);
				}
			}
			Syn::MixinStat => {
				let s_mixin = ast::MixinStat::cast(parent).unwrap();

				if s_mixin.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Macro, range);
				}
			}
			Syn::NativeType => {
				let t_readonly = ast::NativeType::cast(parent).unwrap();

				if t_readonly.ident().is_ok_and(|n| n == token) {
					self.highlight_type_name(token);
				}
			}
			Syn::Parameter => {
				let param = ast::Parameter::cast(parent).unwrap();

				if param.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Param, range);
				}
			}
			Syn::ReadOnlyType => {
				let t_readonly = ast::ReadOnlyType::cast(parent).unwrap();

				if t_readonly.ident().is_ok_and(|n| n == token) {
					self.highlight_type_name(token);
				}
			}
			Syn::ReplacesClause => {
				let clause = ast::ReplacesClause::cast(parent).unwrap();

				if clause.replaced().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Class, range);
				}
			}
			Syn::StatesUsage => {
				self.hl
					.advance_mod(SemToken::Keyword, range, SemTokenFlags::CONTROL_FLOW);
			}
			Syn::StaticConstStat => {
				let s_sc = ast::StaticConstStat::cast(parent).unwrap();

				if s_sc.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Constant, range);
				}
			}
			Syn::StructDef => {
				let structdef = ast::StructDef::cast(parent).unwrap();

				if structdef.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Struct, range);
				}
			}
			Syn::StructExtend => {
				let structext = ast::StructExtend::cast(parent).unwrap();

				if structext.name().is_ok_and(|n| n == token) {
					self.hl.advance(SemToken::Struct, range);
				}
			}
			Syn::VarName => {
				let vn = ast::VarName::cast(parent).unwrap();

				if vn.ident() == token {
					self.hl.advance(SemToken::Property, range);
				}
			}
			_ => {}
		}
	}

	fn highlight_non_ident(&mut self, token: SyntaxToken) {
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
				| Syn::KwWhile => self
					.hl
					.advance_mod(SemToken::Keyword, range, SemTokenFlags::CONTROL_FLOW),
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
				| Syn::KwVolatile => self.hl.advance(SemToken::Keyword, range),
				Syn::KwAlignOf | Syn::KwCross | Syn::KwDot | Syn::KwIs | Syn::KwSizeOf => {
					self.hl.advance(SemToken::Operator, range)
				}
				_ => {}
			};

			return;
		}

		if syn.is_glyph() {
			if let Some(parent) = token.parent() {
				match parent.kind() {
					Syn::BinExpr => {
						let e_bin = ast::BinExpr::cast(parent).unwrap();

						if e_bin.operator().0 == token {
							self.hl.advance(SemToken::Operator, range);
						}
					}
					Syn::PrefixExpr => {
						let e_pre = ast::PrefixExpr::cast(parent).unwrap();

						if e_pre.operator().0 == token {
							self.hl.advance(SemToken::Operator, range);
						}
					}
					Syn::PostfixExpr => {
						let e_post = ast::PostfixExpr::cast(parent).unwrap();

						if e_post.operator().0 == token {
							self.hl.advance(SemToken::Operator, range);
						}
					}
					Syn::TernaryExpr => {
						let e_tern = ast::TernaryExpr::cast(parent).unwrap();

						if e_tern.question_mark() == token
							|| e_tern.colon().is_ok_and(|colon| colon == token)
						{
							self.hl.advance(SemToken::Operator, range);
						}
					}
					_ => {}
				}
			}
		}

		match syn {
			Syn::IntLit | Syn::FloatLit => self.hl.advance(SemToken::Number, range),
			Syn::StringLit => self.hl.advance(SemToken::String, range),
			Syn::NameLit => {
				self.hl.advance(
					SemToken::String,
					TextRange::new(range.start(), range.start() + TextSize::from(1)),
				);
				self.hl.advance(
					SemToken::TypeParam,
					TextRange::new(
						range.start() + TextSize::from(1),
						range.end() - TextSize::from(1),
					),
				);
				self.hl.advance(
					SemToken::String,
					TextRange::new(range.end() - TextSize::from(1), range.end()),
				);
			}
			Syn::NullLit => self.hl.advance(SemToken::Keyword, range),
			Syn::PoundInclude | Syn::RegionStart | Syn::RegionEnd => {
				self.hl
					.advance_mod(SemToken::Keyword, range, SemTokenFlags::CONTROL_FLOW)
			}
			Syn::Comment | Syn::DocComment => self.hl.advance(SemToken::Comment, range),
			// Whitespace, unknown, state sprites, state frames, or previously handled.
			_ => {}
		}
	}

	fn highlight_type_name(&mut self, token: SyntaxToken) {
		let name = self.core.strings.type_name_nocase(token.text());

		let lookup = Project::backwards_lookup(&self.core.projects, self.ix_project, name);

		if let Some((_, sym_k)) = lookup {
			let range = token.text_range();

			match sym_k {
				project::SymbolKey::ZScript(k) => match k {
					SymbolKey::Class(_) => self.hl.advance(SemToken::Class, range),
					SymbolKey::MixinClass(_) => self.hl.advance(SemToken::Macro, range),
					SymbolKey::Struct(_) => self.hl.advance(SemToken::Struct, range),
					SymbolKey::Enum(_) => self.hl.advance(SemToken::Enum, range),
					SymbolKey::Constant(_) => {}
				},
			}

			return;
		}

		const PRIMITIVE_NAMES: &[&str] = &[
			"color",
			"sound",
			"spriteid",
			"statelabel",
			"textureid",
			"voidptr",
		];

		if PRIMITIVE_NAMES
			.iter()
			.any(|n| n.eq_ignore_ascii_case(token.text()))
		{
			self.hl.advance(SemToken::Keyword, token.text_range());
		}
	}
}
