use std::{
	borrow::Borrow,
	ffi::OsStr,
	hash::{Hash, Hasher},
	ops::Deref,
	path::{Path, PathBuf},
};

/// [`PathBuf`] with ASCII-case insensitive equality comparison and hashing.
///
/// Only for use with (G)ZDoom.
#[derive(Debug, Clone)]
pub(crate) struct ZPathBuf(PathBuf);

impl ZPathBuf {
	#[must_use]
	pub(crate) fn new(inner: PathBuf) -> Self {
		Self(inner)
	}

	#[must_use]
	pub(crate) fn into_inner(self) -> PathBuf {
		self.0
	}
}

impl PartialEq for ZPathBuf {
	fn eq(&self, other: &Self) -> bool {
		self.0.as_os_str().eq_ignore_ascii_case(other.0.as_os_str())
	}
}

impl Eq for ZPathBuf {}

impl PartialEq<ZPath> for ZPathBuf {
	fn eq(&self, other: &ZPath) -> bool {
		self.0.as_os_str().eq_ignore_ascii_case(other.0.as_os_str())
	}
}

impl Hash for ZPathBuf {
	fn hash<H: Hasher>(&self, state: &mut H) {
		let string = self.0.to_string_lossy();

		for char in string.chars() {
			u32::from(char.to_ascii_lowercase()).hash(state);
		}
	}
}

impl Borrow<ZPath> for ZPathBuf {
	fn borrow(&self) -> &ZPath {
		self.deref()
	}
}

impl std::ops::Deref for ZPathBuf {
	type Target = ZPath;

	fn deref(&self) -> &Self::Target {
		ZPath::new(self.0.deref())
	}
}

impl<T: ?Sized + AsRef<OsStr>> From<&T> for ZPathBuf {
	fn from(value: &T) -> Self {
		Self(PathBuf::from(value))
	}
}

#[derive(Debug, PartialOrd, Ord)]
pub(crate) struct ZPath(Path);

impl PartialEq for ZPath {
	fn eq(&self, other: &Self) -> bool {
		self.0.as_os_str().eq_ignore_ascii_case(other.0.as_os_str())
	}
}

impl Eq for ZPath {}

impl Hash for ZPath {
	fn hash<H: Hasher>(&self, state: &mut H) {
		let string = self.0.to_string_lossy();

		for char in string.chars() {
			u32::from(char.to_ascii_lowercase()).hash(state);
		}
	}
}

impl ZPath {
	#[must_use]
	pub(crate) fn new<S: AsRef<OsStr> + ?Sized>(string: &S) -> &Self {
		// SAFETY: Same code as `std::path::Path::new`.
		unsafe { &*(string.as_ref() as *const OsStr as *const Self) }
	}

	#[must_use]
	#[allow(unused)]
	pub(crate) fn as_path(&self) -> &Path {
		&self.0
	}
}

impl ToOwned for ZPath {
	type Owned = ZPathBuf;

	fn to_owned(&self) -> Self::Owned {
		ZPathBuf(self.0.to_path_buf())
	}
}

#[cfg(test)]
mod test {
	use crate::FxIndexSet;

	use super::*;

	#[test]
	fn smoke() {
		const P_UPPER: &str = "/home/user/zdoom-project/ZSCRIPT.zs";
		const P_LOWER: &str = "/home/user/zdoom-project/zscript.zs";

		assert_eq!(ZPath::new(P_UPPER), ZPath::new(P_LOWER));

		let mut set = FxIndexSet::default();
		set.insert(ZPathBuf::from(P_UPPER));

		assert!(set.contains(ZPath::new(P_LOWER)));
		let i = set.get_index_of(ZPath::new(P_LOWER));
		assert!(i.is_some());
	}
}
