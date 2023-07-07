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
}

impl PartialEq for ZPathBuf {
	fn eq(&self, other: &Self) -> bool {
		self.0.as_os_str().eq_ignore_ascii_case(other.0.as_os_str())
	}
}

impl Eq for ZPathBuf {}

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
