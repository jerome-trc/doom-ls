use std::rc::Rc;

use doomfront::zdoom::zscript::ast;
use rustc_hash::FxHashSet;

use crate::project::{self, Scope};

use super::sema::*;

#[derive(Debug)]
struct RawNativeClass {
	name: &'static str,
	decl: &'static str,
	doc: &'static str,
	/// Empty for `Object`.
	parent: &'static str,
	structs: &'static [RawNativeStruct],
	enums: &'static [RawNativeEnum],
	/// Includes both fields and constants.
	values: &'static [RawNativeValue],
	/// Includes both statics and methods.
	functions: &'static [RawNativeFunction],
}

#[derive(Debug)]
struct RawNativeEnum {
	name: &'static str,
	decl: &'static str,
	underlying: ast::EnumType,
	doc: &'static str,
	variants: &'static [&'static str],
}

#[derive(Debug)]
struct RawNativeFunction {
	name: &'static str,
	decl: &'static str,
	doc: &'static str,
	is_static: bool,
	is_const: bool,
}

#[derive(Debug)]
struct RawNativePrimitive {
	name: &'static str,
	doc: &'static str,
	/// Includes both fields and constants.
	values: &'static [RawNativeValue],
	/// Includes both statics and methods.
	functions: &'static [RawNativeFunction],
}

#[derive(Debug)]
struct RawNativeStruct {
	name: &'static str,
	decl: &'static str,
	doc: &'static str,
	enums: &'static [RawNativeEnum],
	/// Includes both fields and constants.
	values: &'static [RawNativeValue],
	/// Includes both statics and methods.
	functions: &'static [RawNativeFunction],
}

#[derive(Debug)]
struct RawNativeValue {
	name: &'static str,
	decl: &'static str,
	doc: &'static str,
	kind: ValueKind,
}

/// Documentation and code samples here are provided courtesy of zdoom-docs.
/// For licensing information, see the repository's README file.
const BUILTINS: &[RawNativePrimitive] = &[
	// Integrals ///////////////////////////////////////////////////////////////
	RawNativePrimitive {
		name: "boolean",
		doc: indoc::indoc! {
			"A strongly-typed \"truthy\" value, which can only hold a constant
			`true` or `false`."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "int8",
		doc: indoc::indoc! {
			"A signed 8-bit integer.

			Unlike `int` and `uint`, this can not be used in a function parameter."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "uint8",
		doc: indoc::indoc! {
			"An unsigned 8-bit integer.

			Unlike `int` and `uint`, this can not be used in a function parameter."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "int16",
		doc: indoc::indoc! {
			"A signed 16-bit integer.

			Unlike `int` and `uint`, this can not be used in a function parameter."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "uint16",
		doc: indoc::indoc! {
			"An unsigned 16-bit integer.

			Unlike `int` and `uint`, this can not be used in a function parameter."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "int32",
		doc: indoc::indoc! {
			"A signed 32-bit integer."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "uint32",
		doc: indoc::indoc! {
			"An unsigned 32-bit integer."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "sbyte",
		doc: indoc::indoc! {
			"An alias for `int8`."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "byte",
		doc: indoc::indoc! {
			"An alias for `uint8`."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "short",
		doc: indoc::indoc! {
			"An alias for `int16`."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "ushort",
		doc: indoc::indoc! {
			"An alias for `uint16`."
		},
		values: &[],
		functions: &[],
	},
	// Floating-point //////////////////////////////////////////////////////////
	RawNativePrimitive {
		name: "float",
		doc: indoc::indoc! {
			"A single-precision - i.e. 32-bit - floating-point number.

			Note that when not used as a member variable, `float` is actually
			represented using 64 bits."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "double",
		doc: indoc::indoc! {
			"A double-precision - i.e. 64-bit - floating-point number."
		},
		values: &[],
		functions: &[],
	},
	// Handles and `voidptr` ///////////////////////////////////////////////////
	RawNativePrimitive {
		name: "SpriteID",
		doc: indoc::indoc! {
			"A handle to a sprite."
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "statelabel",
		doc: indoc::indoc! {
			"A handle to a named place in an actor's state machine.",
		},
		values: &[],
		functions: &[],
	},
	RawNativePrimitive {
		name: "voidptr",
		doc: indoc::indoc! {
			"A real memory address. Not available to user code.",
		},
		values: &[],
		functions: &[],
	},
	// TODO: Fields, constants, functions.
];

include!(concat!(env!("OUT_DIR"), "/native/zscript.rs"));

pub(crate) fn register(core: &crate::Core, scope: &mut Scope) {
	for builtin in BUILTINS {
		let iname = core.strings.type_name_nocase(builtin.name);

		scope.insert(
			iname,
			project::Datum::ZScript(Datum::Primitive(PrimitiveDatum {
				name: iname,
				doc: builtin.doc,
				_scope: {
					let mut scope = Scope::default();

					for value in builtin.values {
						register_value(core, &mut scope, value);
					}

					for function in builtin.functions {
						register_function(core, &mut scope, function);
					}

					Rc::new(scope)
				},
			})),
		);
	}

	for function in GLOBAL_FUNCTIONS {
		register_function(core, scope, function);
	}

	for value in GLOBAL_VALUES {
		register_value(core, scope, value);
	}

	for class in CLASSES {
		let iname = core.strings.type_name_nocase(class.name);

		scope.insert(
			iname,
			project::Datum::ZScript(Datum::Class(ClassDatum {
				name: iname,
				source: ClassSource::Native {
					decl: class.decl,
					doc: class.doc,
				},
				scope: {
					let mut scope = Scope::default();

					for structure in class.structs {
						register_struct(core, &mut scope, structure);
					}

					for enumeration in class.enums {
						register_enum(core, &mut scope, enumeration);
					}

					for value in class.values {
						register_value(core, &mut scope, value);
					}

					for function in class.functions {
						register_function(core, &mut scope, function);
					}

					Rc::new(scope)
				},
				parent: {
					if !class.parent.is_empty() {
						Some(core.strings.type_name_nocase(class.parent))
					} else {
						None
					}
				},
			})),
		);
	}

	for enumeration in ENUMS {
		register_enum(core, scope, enumeration);
	}

	for structure in STRUCTS {
		register_struct(core, scope, structure);
	}
}

fn register_enum(core: &crate::Core, scope: &mut Scope, native: &RawNativeEnum) {
	let iname = core.strings.type_name_nocase(native.name);

	scope.insert(
		iname,
		project::Datum::ZScript(Datum::Enum(EnumDatum {
			name: iname,
			source: EnumSource::Native {
				decl: native.decl,
				doc: native.doc,
			},
			underlying: native.underlying,
			variants: {
				let mut set = FxHashSet::default();

				for variant in native.variants {
					set.insert(core.strings.value_name_nocase(variant));
				}

				set
			},
		})),
	);
}

fn register_function(core: &crate::Core, scope: &mut Scope, native: &RawNativeFunction) {
	let iname = core.strings.type_name_nocase(native.name);

	scope.insert(
		iname,
		project::Datum::ZScript(Datum::Function(FunctionDatum {
			name: iname,
			source: FunctionSource::Native {
				signature: native.decl,
				doc: native.doc,
			},
			is_static: native.is_static,
			is_const: native.is_const,
			body: None,
		})),
	);
}

fn register_struct(core: &crate::Core, scope: &mut Scope, native: &RawNativeStruct) {
	let iname = core.strings.type_name_nocase(native.name);

	scope.insert(
		iname,
		project::Datum::ZScript(Datum::Struct(StructDatum {
			name: iname,
			source: StructSource::Native {
				decl: native.decl,
				doc: native.doc,
			},
			scope: {
				let mut scope = Scope::default();

				for enumeration in native.enums {
					register_enum(core, &mut scope, enumeration);
				}

				for value in native.values {
					register_value(core, &mut scope, value);
				}

				for function in native.functions {
					register_function(core, &mut scope, function);
				}

				Rc::new(scope)
			},
		})),
	);
}

fn register_value(core: &crate::Core, scope: &mut Scope, native: &RawNativeValue) {
	let iname = core.strings.type_name_nocase(native.name);

	scope.insert(
		iname,
		project::Datum::ZScript(Datum::Value(ValueDatum {
			name: iname,
			source: ValueSource::Native {
				decl: native.decl,
				doc: native.doc,
			},
			kind: native.kind,
		})),
	);
}
