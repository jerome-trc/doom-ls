use std::{
	borrow::Borrow,
	hash::{Hash, Hasher},
	path::{Path, PathBuf},
};

use append_only_vec::AppendOnlyVec;
use doomfront::rowan::{cursor::SyntaxToken, GreenToken, SyntaxKind};

use crate::FxDashMap;

/// An index into a [`NameInterner`]. Used for symbol lookup.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct NameIx(u32);

/// A [`NameIx`] with a namespace discriminant attached.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum NsName {
	/// These names are registered by CVARINFO files and used for linting
	/// ZScript calls to `CVar.FindCVar` and `CVar.GetCVar`.
	_CVar(NameIx),
	FlagDef(NameIx),
	_Local(NameIx),
	Property(NameIx),
	/// For pseudo-random number generators declared implicitly by ZScript and DECORATE.
	_Prng(NameIx),
	StateLabel(NameIx),
	Type(NameIx),
	Value(NameIx),
}

impl NsName {
	#[must_use]
	#[allow(unused)]
	fn index(self) -> NameIx {
		match self {
			Self::_CVar(ix)
			| Self::FlagDef(ix)
			| Self::_Local(ix)
			| Self::Property(ix)
			| Self::_Prng(ix)
			| Self::StateLabel(ix)
			| Self::Type(ix)
			| Self::Value(ix) => ix,
		}
	}
}

/// A concurrent interner for [`IName`],
/// allowing 32-bit indices to be used as map keys in place of pointers.
#[derive(Debug)]
pub(crate) struct NameInterner {
	array: AppendOnlyVec<IName>,
	map: FxDashMap<IName, NameIx>,
}

impl NameInterner {
	#[must_use]
	pub(crate) fn intern(&self, token: &SyntaxToken) -> NameIx {
		let green = unsafe { std::mem::transmute::<_, &GreenTokenData>(token.green()) };

		if let Some(kvp) = self.map.get(green) {
			return *kvp.value();
		}

		self.add(green.0.to_owned())
	}

	#[must_use]
	pub(crate) fn intern_str(&self, string: &str) -> NameIx {
		if let Some(kvp) = self.map.get(string) {
			return *kvp.value();
		}

		self.add(GreenToken::new(SyntaxKind(0), string))
	}

	#[must_use]
	pub(crate) fn resolve(&self, ns_name: NsName) -> &str {
		self.array[ns_name.index().0 as usize].0.text()
	}

	#[must_use]
	fn add(&self, token: GreenToken) -> NameIx {
		let v = IName(token);
		let ix = self.array.push(v.clone());
		debug_assert!(ix < (u32::MAX as usize));
		let ret = NameIx(ix as u32);
		self.map.insert(v, ret);
		ret
	}
}

impl Default for NameInterner {
	fn default() -> Self {
		Self {
			array: AppendOnlyVec::new(),
			map: FxDashMap::default(),
		}
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

#[derive(Debug)]
pub(crate) struct PathInterner {
	array: AppendOnlyVec<PathArc>,
	map: FxDashMap<PathArc, PathIx>,
}

impl PathInterner {
	#[must_use]
	pub(crate) fn intern(&self, path: &Path) -> PathIx {
		if let Some(kvp) = self.map.get(path) {
			return *kvp.value();
		}

		self.add(PathArc::from(path))
	}

	#[must_use]
	pub(crate) fn intern_owned(&self, pathbuf: PathBuf) -> PathIx {
		if let Some(kvp) = self.map.get(pathbuf.as_path()) {
			return *kvp.value();
		}

		self.add(PathArc::from(pathbuf))
	}

	#[must_use]
	fn add(&self, path: PathArc) -> PathIx {
		let ix = self.array.push(path.clone());
		debug_assert!(ix < (u32::MAX as usize));
		let ret = PathIx(ix as u32);
		self.map.insert(path, ret);
		ret
	}

	#[must_use]
	pub(crate) fn resolve(&self, ix: PathIx) -> &Path {
		self.array[ix.0 as usize].as_path()
	}
}

impl Default for PathInterner {
	fn default() -> Self {
		Self {
			array: AppendOnlyVec::new(),
			map: FxDashMap::default(),
		}
	}
}

#[derive(Clone)]
struct PathArc(triomphe::ThinArc<(), u8>);

impl std::fmt::Debug for PathArc {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "\"{}\"", self.as_str())
	}
}

impl PathArc {
	#[must_use]
	fn as_str(&self) -> &str {
		// SAFETY: All constructions of this type go through `to_string_lossy`.
		unsafe { std::str::from_utf8_unchecked(&self.0.slice) }
	}

	#[must_use]
	fn as_path(&self) -> &Path {
		Path::new(self.as_str())
	}
}

impl From<&Path> for PathArc {
	fn from(value: &Path) -> Self {
		Self(triomphe::ThinArc::from_header_and_slice(
			(),
			value.to_string_lossy().as_bytes(),
		))
	}
}

impl From<PathBuf> for PathArc {
	fn from(value: PathBuf) -> Self {
		Self(triomphe::ThinArc::from_header_and_slice(
			(),
			value.to_string_lossy().as_bytes(),
		))
	}
}

impl std::borrow::Borrow<Path> for PathArc {
	fn borrow(&self) -> &Path {
		self.as_path()
	}
}

impl PartialEq<Path> for PathArc {
	fn eq(&self, other: &Path) -> bool {
		std::borrow::Borrow::<Path>::borrow(self) == other
	}
}

impl PartialEq for PathArc {
	fn eq(&self, other: &Self) -> bool {
		std::borrow::Borrow::<Path>::borrow(self) == std::borrow::Borrow::<Path>::borrow(other)
	}
}

impl Eq for PathArc {}

impl Hash for PathArc {
	fn hash<H: Hasher>(&self, state: &mut H) {
		std::borrow::Borrow::<Path>::borrow(self).hash(state)
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn smoke_paths() {
		let paths = PathInterner::default();
		let i0 = paths.intern(Path::new("/home/user/my_doom_mod/ZSCRIPT.zs"));
		let i1 = paths.intern(Path::new("/home/user/my_doom_mod/ZSCRIPT.zs"));
		assert_eq!(i0, i1);
	}
}
