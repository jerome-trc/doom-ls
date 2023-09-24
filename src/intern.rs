use std::{
	borrow::Borrow,
	hash::{Hash, Hasher}, path::{PathBuf, Path},
};

use doomfront::rowan::{cursor::SyntaxToken, GreenToken};

use crate::FxIndexSet;

/// An index into a [`NameInterner`]. Used for symbol lookup.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct NameIx(u32);

/// A [`NameIx`] with a namespace discriminant attached.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum NsName {
	CVar(NameIx),
	FlagDef(NameIx),
	Property(NameIx),
	/// "Pseudo-random number generator".
	Prng(NameIx),
	Type(NameIx),
	Value(NameIx),
}

/// A concurrent interner for [`IName`],
/// allowing 32-bit indices to be used as map keys in place of pointers.
#[derive(Debug, Default)]
pub(crate) struct NameInterner {
	map: FxIndexSet<IName>,
}

impl NameInterner {
	#[must_use]
	pub(crate) fn intern(&mut self, token: &SyntaxToken) -> NameIx {
		let green = unsafe { std::mem::transmute::<_, &GreenTokenData>(token.green()) };

		if let Some(ix) = self.map.get_index_of(green) {
			return NameIx(ix as u32);
		}

		let (ix, _) = self.map.insert_full(IName(green.0.to_owned()));
		NameIx(ix as u32)
	}

	#[must_use]
	pub(crate) fn resolve(&self, ix: NameIx) -> &str {
		self.map[ix.0 as usize].0.text()
	}
}

// IName ///////////////////////////////////////////////////////////////////////

/// "Interned name"; a [`GreenToken`] with case-insensitive comparison and hashing.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub(crate) struct IName(pub(crate) GreenToken);

impl PartialEq for IName {
	fn eq(&self, other: &Self) -> bool {
		let self_text: &str = self.borrow();
		let o_text: &str = other.borrow();
		self_text.eq_ignore_ascii_case(o_text)
	}
}

impl Eq for IName {}

impl Hash for IName {
	fn hash<H: Hasher>(&self, state: &mut H) {
		let text: &str = self.borrow();

		for c in text.chars() {
			c.to_ascii_lowercase().hash(state);
		}
	}
}

impl Borrow<str> for IName {
	fn borrow(&self) -> &str {
		let whole = self.0.text();

		if whole.ends_with('\'') {
			// Name literal
			&whole[1..(whole.len() - 1)]
		} else if whole.ends_with('"') {
			// String literal
			let start = whole.chars().position(|c| c == '"').unwrap();
			&whole[(start + 1)..(whole.len() - 1)]
		} else {
			whole
		}
	}
}

impl From<GreenToken> for IName {
	fn from(value: GreenToken) -> Self {
		Self(value)
	}
}

impl From<&GreenToken> for IName {
	fn from(value: &GreenToken) -> Self {
		Self(value.clone())
	}
}

impl Borrow<GreenTokenData> for IName {
	fn borrow(&self) -> &GreenTokenData {
		unsafe {
			std::mem::transmute::<&doomfront::rowan::GreenTokenData, &GreenTokenData>(
				self.0.borrow(),
			)
		}
	}
}

impl std::fmt::Display for IName {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.0.text().fmt(f)
	}
}

#[derive(Debug)]
#[repr(transparent)]
struct GreenTokenData(doomfront::rowan::GreenTokenData);

impl PartialEq for GreenTokenData {
	fn eq(&self, other: &Self) -> bool {
		self.0.text().eq_ignore_ascii_case(other.0.text())
	}
}

impl Eq for GreenTokenData {}

impl Hash for GreenTokenData {
	fn hash<H: Hasher>(&self, state: &mut H) {
		for c in self.0.text().chars() {
			c.to_ascii_lowercase().hash(state);
		}
	}
}

// PathInterner ////////////////////////////////////////////////////////////////

/// An index into a [`PathInterner`]. Acts as a unique identifier for files.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct PathIx(u32);

#[derive(Debug, Default)]
pub(crate) struct PathInterner {
	map: FxIndexSet<PathBuf>,
}

impl PathInterner {
	#[must_use]
	pub(crate) fn intern(&mut self, path: &Path) -> PathIx {
		if let Some(ix) = self.map.get_index_of(path) {
			return PathIx(ix as u32);
		}

		let (ix, _) = self.map.insert_full(path.to_owned());
		PathIx(ix as u32)
	}

	#[must_use]
	pub(crate) fn intern_owned(&mut self, pathbuf: PathBuf) -> PathIx {
		if let Some(ix) = self.map.get_index_of(&pathbuf) {
			return PathIx(ix as u32);
		}

		let (ix, _) = self.map.insert_full(pathbuf);
		PathIx(ix as u32)
	}

	#[must_use]
	pub(crate) fn resolve(&self, ix: PathIx) -> &Path {
		self.map[ix.0 as usize].as_path()
	}
}
