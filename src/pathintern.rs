//! [`PathInterner`] and [`PathIx`].

use std::{
	borrow::Borrow,
	hash::{Hash, Hasher},
	path::{Path, PathBuf},
};

use vtutil::pushvec::PushVec;

use crate::FxDashMap;

/// An index into a [`PathInterner`]. Acts as a unique identifier for files.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PathIx(u32);

/// A concurrent interner for filesystem paths, allowing [32-bit indices](PathIx)
/// to be used as map keys in place of pointers.
#[derive(Debug, Default)]
pub struct PathInterner {
	array: PushVec<PathArc>,
	map: FxDashMap<PathArc, PathIx>,
}

impl PathInterner {
	#[must_use]
	pub fn intern(&self, path: &Path) -> PathIx {
		self.add(PathArc::from(path))
	}

	#[must_use]
	pub fn intern_owned(&self, pathbuf: PathBuf) -> PathIx {
		self.add(PathArc::from(pathbuf))
	}

	#[must_use]
	fn add(&self, path: PathArc) -> PathIx {
		let vac = match self.map.entry(path.clone()) {
			dashmap::mapref::entry::Entry::Occupied(occ) => return *occ.get(),
			dashmap::mapref::entry::Entry::Vacant(vac) => vac,
		};

		let ix = self.array.push(path.clone());
		debug_assert!(ix < (u32::MAX as usize));
		let ret = PathIx(ix as u32);
		vac.insert(ret);
		ret
	}

	#[must_use]
	pub fn resolve(&self, ix: PathIx) -> &Path {
		self.array[ix.0 as usize].as_path()
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

impl Borrow<Path> for PathArc {
	fn borrow(&self) -> &Path {
		self.as_path()
	}
}

impl PartialEq<Path> for PathArc {
	fn eq(&self, other: &Path) -> bool {
		Borrow::<Path>::borrow(self) == other
	}
}

impl PartialEq for PathArc {
	fn eq(&self, other: &Self) -> bool {
		Borrow::<Path>::borrow(self) == Borrow::<Path>::borrow(other)
	}
}

impl Eq for PathArc {}

impl Hash for PathArc {
	fn hash<H: Hasher>(&self, state: &mut H) {
		Borrow::<Path>::borrow(self).hash(state)
	}
}
