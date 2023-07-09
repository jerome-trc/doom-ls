//! Name interning and ASCII case-insensitive string utilities.

use std::{
	borrow::Borrow,
	cell::RefCell,
	hash::{Hash, Hasher},
	ops::Deref,
};

use crate::FxIndexSet;

#[derive(Debug, Default)]
pub(crate) struct StringInterner {
	_cased: RefCell<FxIndexSet<Box<str>>>,
	nocase: RefCell<FxIndexSet<ZString>>,
}

impl StringInterner {
	pub(crate) fn _get_or_intern_cased(&self, string: &str) -> StringKey {
		let mut set = self._cased.borrow_mut();

		match set.get_full(string) {
			Some((i, _)) => StringKey(i as u32),
			None => StringKey(set.insert_full(string.to_owned().into_boxed_str()).0 as u32),
		}
	}

	#[must_use]
	pub(crate) fn get_or_intern_nocase(&self, string: &str) -> StringKey {
		let mut set = self.nocase.borrow_mut();

		match set.get_full(ZStr::new(string)) {
			Some((i, _)) => StringKey(i as u32),
			None => StringKey(
				set.insert_full(ZString(string.to_owned().into_boxed_str()))
					.0 as u32,
			),
		}
	}

	#[must_use]
	pub(crate) fn type_name_nocase(&self, string: &str) -> Name {
		Name::Type(self.get_or_intern_nocase(string))
	}

	#[must_use]
	pub(crate) fn var_name_nocase(&self, string: &str) -> Name {
		Name::Variable(self.get_or_intern_nocase(string))
	}
}

/// An index to a string interned into a [`StringInterner`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StringKey(u32);

/// A [`StringKey`] tagged with a namespace.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Name {
	Type(StringKey),
	Variable(StringKey),
	// TODO: CVars, GLDEFS objects, et cetera...
}

/// A string with ASCII case-insensitive equality comparison and hashing.
///
/// Only for use when dealing with (G)ZDoom.
#[derive(Debug, Clone)]
struct ZString(Box<str>);

impl PartialEq for ZString {
	fn eq(&self, other: &Self) -> bool {
		self.0.eq_ignore_ascii_case(&other.0)
	}
}

impl PartialEq<ZStr> for ZString {
	fn eq(&self, other: &ZStr) -> bool {
		self.0.eq_ignore_ascii_case(&other.0)
	}
}

impl Eq for ZString {}

impl Hash for ZString {
	fn hash<H: Hasher>(&self, state: &mut H) {
		for char in self.0.chars() {
			u32::from(char.to_ascii_lowercase()).hash(state);
		}
	}
}

impl Borrow<ZStr> for ZString {
	fn borrow(&self) -> &ZStr {
		self.deref()
	}
}

impl Deref for ZString {
	type Target = ZStr;

	fn deref(&self) -> &Self::Target {
		ZStr::new(self.0.deref())
	}
}

#[derive(Debug)]
struct ZStr(str);

impl ZStr {
	#[must_use]
	pub(crate) fn new<S: AsRef<str> + ?Sized>(string: &S) -> &Self {
		// SAFETY: Same code as `std::path::Path::new`.
		unsafe { &*(string.as_ref() as *const str as *const Self) }
	}
}

impl PartialEq for ZStr {
	fn eq(&self, other: &Self) -> bool {
		self.0.eq_ignore_ascii_case(&other.0)
	}
}

impl Eq for ZStr {}

impl Hash for ZStr {
	fn hash<H: Hasher>(&self, state: &mut H) {
		for char in self.0.chars() {
			u32::from(char.to_ascii_lowercase()).hash(state);
		}
	}
}

#[cfg(test)]
mod test {
	use crate::FxIndexSet;

	use super::*;

	#[test]
	fn smoke_strings() {
		let mut set = FxIndexSet::default();
		set.insert(ZString("hello world".to_string().into_boxed_str()));
		assert!(set.contains(ZStr::new("HELLO WORLD")));
	}
}
