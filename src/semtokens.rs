//! Abstractions over [`lsp_types::SemanticToken`] and [`lsp_types::SemanticTokenModifier`].

use doomfront::rowan::TextRange;
use lsp_types::{SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend};

use crate::lines::LineIndex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SemToken {
	Comment,
	Constant,
	Enum,
	Function,
	Keyword,
	Method,
	Number,
	Operator,
	Param,
	Property,
	String,
	Struct,
	Type,
	TypeParam,
}

impl From<SemToken> for SemanticTokenType {
	fn from(value: SemToken) -> Self {
		match value {
			SemToken::Comment => Self::COMMENT,
			SemToken::Constant => Self::ENUM_MEMBER,
			SemToken::Enum => Self::ENUM,
			SemToken::Function => Self::FUNCTION,
			SemToken::Keyword => Self::KEYWORD,
			SemToken::Method => Self::METHOD,
			SemToken::Number => Self::NUMBER,
			SemToken::Operator => Self::OPERATOR,
			SemToken::Param => Self::PARAMETER,
			SemToken::Property => Self::PROPERTY,
			SemToken::String => Self::STRING,
			SemToken::Struct => Self::STRUCT,
			SemToken::Type => Self::TYPE,
			SemToken::TypeParam => Self::TYPE_PARAMETER,
		}
	}
}

bitflags::bitflags! {
	#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
	pub(crate) struct SemTokenFlags: u32 {
		const DEPRECATED = 1 << 0;
		const READONLY = 1 << 1;
		const CONTROL_FLOW = 1 << 2;
	}
}

pub(crate) struct Highlighter<'li> {
	pub(crate) lndx: &'li LineIndex,
	pub(crate) tokens: Vec<lsp_types::SemanticToken>,
	pub(crate) prev_line: u32,
	pub(crate) prev_col: u32,
}

impl<'li> Highlighter<'li> {
	#[must_use]
	pub(crate) fn new(lndx: &'li LineIndex) -> Self {
		Self {
			lndx,
			tokens: vec![],
			prev_line: 0,
			prev_col: 0,
		}
	}

	pub(crate) fn advance_mod(&mut self, semtok: SemToken, range: TextRange, flags: SemTokenFlags) {
		self.advance_impl(semtok, range, flags.bits());
	}

	pub(crate) fn advance(&mut self, semtok: SemToken, range: TextRange) {
		self.advance_impl(semtok, range, SemTokenFlags::empty().bits())
	}

	fn advance_impl(&mut self, semtok: SemToken, range: TextRange, bits: u32) {
		let linecol = self.lndx.line_col(range.start());
		let mut c = linecol;

		if !self.tokens.is_empty() {
			c.line -= self.prev_line;

			if c.line == 0 {
				c.col -= self.prev_col;
			}
		}

		self.tokens.push(lsp_types::SemanticToken {
			delta_line: c.line,
			delta_start: c.col,
			length: range.len().into(),
			token_type: semtok as u32,
			token_modifiers_bitset: bits,
		});

		self.prev_line = linecol.line;
		self.prev_col = linecol.col;
	}
}

#[must_use]
pub(crate) fn legend() -> SemanticTokensLegend {
	// Ordering must match that of `SemToken`.
	let types = vec![
		SemToken::Comment.into(),
		SemToken::Constant.into(),
		SemToken::Enum.into(),
		SemToken::Function.into(),
		SemToken::Keyword.into(),
		SemToken::Method.into(),
		SemToken::Number.into(),
		SemToken::Operator.into(),
		SemToken::Param.into(),
		SemToken::Property.into(),
		SemToken::String.into(),
		SemToken::Struct.into(),
		SemToken::Type.into(),
		SemToken::TypeParam.into(),
	];

	let modifiers = vec![
		SemanticTokenModifier::DEPRECATED,
		SemanticTokenModifier::READONLY,
		SemanticTokenModifier::new("controlFlow"),
	];

	SemanticTokensLegend {
		token_types: types,
		token_modifiers: modifiers,
	}
}
