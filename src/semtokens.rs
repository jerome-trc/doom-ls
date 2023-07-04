//! Abstractions over [`lsp_types::SemanticToken`] and [`lsp_types::SemanticTokenModifier`].

use std::sync::Arc;

use doomfront::rowan::{TextRange, TextSize};
use lsp_types::{SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SemToken {
	Class,
	Comment,
	Constant,
	Enum,
	Function,
	Keyword,
	Method,
	Modifier,
	Number,
	Operator,
	Param,
	Property,
	String,
	Struct,
	Type,
	TypeParam,
	Variable,
}

impl From<SemToken> for SemanticTokenType {
	fn from(value: SemToken) -> Self {
		match value {
			SemToken::Class => Self::CLASS,
			SemToken::Comment => Self::COMMENT,
			SemToken::Constant => Self::ENUM_MEMBER,
			SemToken::Enum => Self::ENUM,
			SemToken::Function => Self::FUNCTION,
			SemToken::Keyword => Self::KEYWORD,
			SemToken::Method => Self::METHOD,
			SemToken::Modifier => Self::MODIFIER,
			SemToken::Number => Self::NUMBER,
			SemToken::Operator => Self::OPERATOR,
			SemToken::Param => Self::PARAMETER,
			SemToken::Property => Self::PROPERTY,
			SemToken::String => Self::STRING,
			SemToken::Struct => Self::STRUCT,
			SemToken::Type => Self::TYPE,
			SemToken::TypeParam => Self::TYPE_PARAMETER,
			SemToken::Variable => Self::VARIABLE,
		}
	}
}

bitflags::bitflags! {
	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	pub(crate) struct SemTokenFlags: u32 {
		const DEPRECATED = 1 << 0;
		const READONLY = 1 << 1;
	}
}

pub(crate) struct Highlighter {
	pub(crate) newlines: Arc<[TextSize]>,
	pub(crate) tokens: Vec<lsp_types::SemanticToken>,
	pub(crate) cur_line: u32,
	pub(crate) cur_char: u32,
}

impl Highlighter {
	#[must_use]
	pub(crate) fn new(newlines: Arc<[TextSize]>) -> Self {
		Self {
			newlines,
			tokens: vec![],
			cur_line: 0,
			cur_char: 0,
		}
	}

	pub(crate) fn _advance_mod(
		&mut self,
		semtok: SemToken,
		range: TextRange,
		flags: SemTokenFlags,
	) {
		self.advance_impl(semtok, range, flags.bits());
	}

	pub(crate) fn advance(&mut self, semtok: SemToken, range: TextRange) {
		self.advance_impl(semtok, range, SemTokenFlags::empty().bits())
	}

	fn advance_impl(&mut self, semtok: SemToken, range: TextRange, bits: u32) {
		let mut l = self.cur_line;
		let mut loffs = 0_u32;

		while let Some(&newline) = self.newlines.get(l as usize) {
			if newline < range.start() {
				l += 1;
			} else {
				break;
			}

			loffs = newline.into();
		}

		let ladv = l > self.cur_line;
		let start_char = u32::from(range.start()) - loffs;

		self.tokens.push(lsp_types::SemanticToken {
			delta_line: {
				let dl = l - self.cur_line;
				self.cur_line = l;
				dl
			},
			delta_start: {
				if ladv {
					self.cur_char = start_char;
					start_char
				} else {
					let ds = start_char - self.cur_char;
					self.cur_char = start_char;
					ds
				}
			},
			length: range.len().into(),
			token_type: semtok as u32,
			token_modifiers_bitset: bits,
		});
	}
}

#[must_use]
pub(crate) fn legend() -> SemanticTokensLegend {
	// Ordering must match that of `SemToken`.
	let types = vec![
		SemToken::Class.into(),
		SemToken::Comment.into(),
		SemToken::Constant.into(),
		SemToken::Enum.into(),
		SemToken::Function.into(),
		SemToken::Keyword.into(),
		SemToken::Method.into(),
		SemToken::Modifier.into(),
		SemToken::Number.into(),
		SemToken::Operator.into(),
		SemToken::Param.into(),
		SemToken::Property.into(),
		SemToken::String.into(),
		SemToken::Struct.into(),
		SemToken::Type.into(),
		SemToken::TypeParam.into(),
		SemToken::Variable.into(),
	];

	let modifiers = vec![
		SemanticTokenModifier::DEPRECATED,
		SemanticTokenModifier::READONLY,
	];

	SemanticTokensLegend {
		token_types: types,
		token_modifiers: modifiers,
	}
}
