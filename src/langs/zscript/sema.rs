//! Semantic information types.

use doomfront::zdoom::{self, zscript::ast::EnumType};

use crate::{
	core::Core,
	data::{SymPtr, Symbol},
	intern::NsName,
};

#[derive(Debug)]
pub(crate) enum Datum {
	Class(Class),
	Constant,
	Enum(Enumeration),
	Field(Field),
	Function(Function),
	_MixinClass,
	Primitive(Primitive),
	Struct(Structure),
}

#[derive(Debug)]
pub(crate) enum QualType {
	Normal { ptr: SymPtr, readonly: bool },
	StaticArray { elem_t: SymPtr, len: usize },
	DynArray { elem_t: SymPtr },
	Map { key_t: SymPtr, val_t: SymPtr },
	MapIter { key_t: SymPtr, val_t: SymPtr },
}

impl QualType {
	pub(crate) fn format(&self, ctx: &Core, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Normal { ptr, readonly } => {
				let tname = match ptr.as_ref().unwrap() {
					Symbol::User(u_t) => ctx.names.resolve(u_t.name),
					Symbol::Internal(in_t) => in_t.name,
				};

				if !*readonly {
					write!(f, "{tname}")
				} else {
					write!(f, "readonly<{tname}>")
				}
			}
			Self::StaticArray { elem_t, len } => {
				let tname = match elem_t.as_ref().unwrap() {
					Symbol::User(u_t) => ctx.names.resolve(u_t.name),
					Symbol::Internal(in_t) => in_t.name,
				};

				write!(f, "{tname}[{len}]")
			}
			Self::DynArray { elem_t } => {
				let tname = match elem_t.as_ref().unwrap() {
					Symbol::User(u_t) => ctx.names.resolve(u_t.name),
					Symbol::Internal(in_t) => in_t.name,
				};

				write!(f, "array<{tname}>")
			}
			Self::Map { key_t, val_t } => {
				let key_t_name = match key_t.as_ref().unwrap() {
					Symbol::User(u_t) => ctx.names.resolve(u_t.name),
					Symbol::Internal(in_t) => in_t.name,
				};

				let val_t_name = match val_t.as_ref().unwrap() {
					Symbol::User(u_t) => ctx.names.resolve(u_t.name),
					Symbol::Internal(in_t) => in_t.name,
				};

				write!(f, "map<{key_t_name}, {val_t_name}>")
			}
			Self::MapIter { key_t, val_t } => {
				let key_t_name = match key_t.as_ref().unwrap() {
					Symbol::User(u_t) => ctx.names.resolve(u_t.name),
					Symbol::Internal(in_t) => in_t.name,
				};

				let val_t_name = match val_t.as_ref().unwrap() {
					Symbol::User(u_t) => ctx.names.resolve(u_t.name),
					Symbol::Internal(in_t) => in_t.name,
				};

				write!(f, "mapIterator<{key_t_name}, {val_t_name}>")
			}
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Scope {
	Data,
	Ui,
	Play,
	Virtual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Visibility {
	Public,
	Protected,
	Private,
}

#[derive(Debug)]
pub(crate) struct Class {
	pub(crate) flags: ClassFlags,
	pub(crate) scope: Scope,
	pub(crate) min_version: Option<zdoom::Version>,
}

bitflags::bitflags! {
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	pub(crate) struct ClassFlags: u8 {
		const ABSTRACT = 1 << 0;
		const NATIVE = 1 << 1;
	}
}

#[derive(Debug)]
pub(crate) struct Enumeration {
	pub(crate) underlying: EnumType,
}

#[derive(Debug)]
pub(crate) struct Field {
	pub(crate) flags: FieldFlags,
	pub(crate) scope: Scope,
	pub(crate) deprecated: Option<(zdoom::Version, String)>,
}

bitflags::bitflags! {
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	pub(crate) struct FieldFlags: u16 {
		const NATIVE = 1 << 0;
		const META = 1 << 1;
		const TRANSIENT = 1 << 2;
		const READONLY = 1 << 3;
	}
}

#[derive(Debug)]
pub(crate) struct Function {
	pub(crate) params: Vec<Parameter>,
	pub(crate) flags: FunctionFlags,
	pub(crate) scope: Scope,
	pub(crate) min_version: Option<zdoom::Version>,
	pub(crate) vis: Visibility,
	pub(crate) deprecated: Option<(zdoom::Version, String)>,
}

bitflags::bitflags! {
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	pub(crate) struct FunctionFlags: u16 {
		const ABSTRACT = 1 << 0;
		const CONST = 1 << 1;
		const FINAL = 1 << 2;
		const NATIVE = 1 << 3;
		const OVERRIDE = 1 << 4;
		const STATIC = 1 << 5;
		const VARARGS = 1 << 6;
		const VIRTUAL = 1 << 7;

		const ACTION = 1 << 8;
		const ACTION_ACTOR = 1 << 9;
		const ACTION_ITEM = 1 << 10;
		const ACTION_OVERLAY = 1 << 11;
		const ACTION_WEAPON = 1 << 12;
	}
}

#[derive(Debug)]
pub(crate) enum Primitive {
	Integer { bits: u8, signed: bool },
	Float,
	Double,
	Bool,
	SpriteId,
	StateLabel,
	Voidptr,
}

#[derive(Debug)]
pub(crate) struct Structure {
	pub(crate) scope: Scope,
	pub(crate) flags: StructFlags,
	pub(crate) min_version: Option<zdoom::Version>,
}

bitflags::bitflags! {
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	pub(crate) struct StructFlags: u8 {
		const NATIVE = 1 << 0;
	}
}

#[derive(Debug)]
pub(crate) struct Parameter {
	pub(crate) name: NsName,
	pub(crate) qtype: QualType,
	pub(crate) in_ref: bool,
	pub(crate) out_ref: bool,
}
