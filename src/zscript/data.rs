//! Interned values and their key newtypes.

use crate::project::FilePos;

// Class ///////////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct ClassDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ClassKey(salsa::InternId);

impl salsa::InternKey for ClassKey {
	fn from_intern_id(v: salsa::InternId) -> Self {
		Self(v)
	}

	fn as_intern_id(&self) -> salsa::InternId {
		self.0
	}
}

// Symbolic constant ///////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct ConstantDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ConstantKey(salsa::InternId);

impl salsa::InternKey for ConstantKey {
	fn from_intern_id(v: salsa::InternId) -> Self {
		Self(v)
	}

	fn as_intern_id(&self) -> salsa::InternId {
		self.0
	}
}

// Enumeration /////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct EnumDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct EnumKey(salsa::InternId);

impl salsa::InternKey for EnumKey {
	fn from_intern_id(v: salsa::InternId) -> Self {
		Self(v)
	}

	fn as_intern_id(&self) -> salsa::InternId {
		self.0
	}
}

// Mixin class /////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct MixinClassDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct MixinClassKey(salsa::InternId);

impl salsa::InternKey for MixinClassKey {
	fn from_intern_id(v: salsa::InternId) -> Self {
		Self(v)
	}

	fn as_intern_id(&self) -> salsa::InternId {
		self.0
	}
}

// Struct //////////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct StructDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StructKey(salsa::InternId);

impl salsa::InternKey for StructKey {
	fn from_intern_id(v: salsa::InternId) -> Self {
		Self(v)
	}

	fn as_intern_id(&self) -> salsa::InternId {
		self.0
	}
}
