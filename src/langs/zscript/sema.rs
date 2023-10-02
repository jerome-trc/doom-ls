//! Semantic information types.

use doomfront::zdoom;

#[derive(Debug)]
pub(crate) enum Datum {
	Class,
	Constant,
	_Field(Field),
	Function(Function),
}

#[derive(Debug)]
pub(crate) struct Field {
	pub(crate) flags: FieldFlags,
	pub(crate) deprecated: Option<(zdoom::Version, String)>,
}

#[derive(Debug)]
pub(crate) struct Function {
	pub(crate) flags: FunctionFlags,
	pub(crate) scope: Scope,
	pub(crate) _min_version: zdoom::Version,
	pub(crate) vis: Visibility,
	pub(crate) deprecated: Option<(zdoom::Version, String)>,
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

bitflags::bitflags! {
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	pub(crate) struct FunctionFlags: u16 {
		const ABSTRACT = 1 << 0;
		const ACTION = 1 << 1;
		const CONST = 1 << 2;
		const FINAL = 1 << 3;
		const NATIVE = 1 << 4;
		const OVERRIDE = 1 << 5;
		const STATIC = 1 << 6;
		const VARARGS = 1 << 7;
		const VIRTUAL = 1 << 8;
	}
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
