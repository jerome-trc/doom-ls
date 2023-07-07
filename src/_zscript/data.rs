use slotmap::SlotMap;

use crate::_project::FilePos;

#[derive(Debug, Default)]
pub(crate) struct Storage {
	data: SlotMap<DatumKey, Datum>,
}

slotmap::new_key_type! { pub(crate) struct DatumKey; }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SymbolKey {
	Class(DatumKey),
	Constant(DatumKey),
	Enum(DatumKey),
	MixinClass(DatumKey),
}

#[derive(Debug)]
pub(crate) enum Datum {
	Class(ClassDatum),
	Constant(ConstantDatum),
	Enum(EnumDatum),
	MixinClass(MixinClassDatum),
	Struct(StructDatum),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct ClassDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct ConstantDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct EnumDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct MixinClassDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct StructDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}
