//! Code common to every language frontend.

pub mod intern;
pub mod mem;
pub mod sym;

use doomfront::rowan::{TextRange, TextSize};

use crate::pathintern::PathIx;

use self::intern::NameIx;

/// A unique location across all files.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileSpan {
	pub file_id: PathIx,
	pub span: TextRange,
}

impl PartialOrd for FileSpan {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		match self.file_id.partial_cmp(&other.file_id) {
			Some(core::cmp::Ordering::Equal) => {}
			ord => return ord,
		}

		PartialOrd::partial_cmp(
			&(u32::from(self.span.start()), u32::from(self.span.end())),
			&(u32::from(other.span.start()), u32::from(other.span.end())),
		)
	}
}

impl Ord for FileSpan {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		match self.file_id.cmp(&other.file_id) {
			core::cmp::Ordering::Equal => {}
			ord => return ord,
		}

		Ord::cmp(
			&(u32::from(self.span.start()), u32::from(self.span.end())),
			&(u32::from(other.span.start()), u32::from(other.span.end())),
		)
	}
}

/// A unique identifier for a [symbol](sym::Symbol).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId {
	pub file_id: PathIx,
	pub offs: TextSize,
}

/// A [`NameIx`] with a namespace discriminant attached.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NsName {
	Type(NameIx),
	Value(NameIx),
}
