//! ZScript internal symbol registration.
//!
//! Even if it were good UX to expect users to unzip their gzdoom.pk3 and point
//! the server's configuration to it, it would not account for compiler intrinsics
//! and primitives and so on. All those symbols are instead emitted from here.

use crate::{
	arena::Arena,
	core::Scope,
	data::{Definition, InternalSymbol, SymPtr, Symbol},
	intern::{NameInterner, NsName},
	langs::LangId,
};

use super::sema::{self, ClassFlags, FunctionFlags};

#[derive(Debug)]
pub(crate) struct Cache {
	pub(crate) class_object: SymPtr,
}

#[must_use]
pub(crate) fn register(
	names: &NameInterner,
	globals: &mut Scope,
	bump: &mut bumpalo::Bump,
) -> Cache {
	register_primitives(names, globals, bump);

	let ptr_class_object = register_with_members(
		globals,
		bump,
		[
			(
				NsName::Value(names.intern_str("TICRATE")),
				InternalSymbol {
					lang: LangId::ZScript,
					name: "TICRATE",
					decl: "const Object.TICRATE = 35",
					docs: &[
						r#"The "simulation rate" of the game; that is, the number of updates or "ticks" in a second."#,
					],
					scope: None,
					def: Definition::ZScript(sema::Datum::Constant),
				},
			),
			(
				NsName::Value(names.intern_str("GetClass")),
				InternalSymbol {
					lang: LangId::ZScript,
					name: "GetClass",
					decl: "virtualScope class<Object> Object.GetClass()",
					docs: &["Returns the class type of this object."],
					scope: None,
					def: Definition::ZScript(sema::Datum::Function(sema::Function {
						params: vec![],
						scope: sema::Scope::Virtual,
						flags: FunctionFlags::empty(),
						vis: sema::Visibility::Public,
						min_version: None,
						deprecated: None,
					})),
				},
			),
		],
		NsName::Type(names.intern_str("Object")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "Object",
			decl: "class Object native",
			docs: &["The base class of all classes except itself."],
			scope: None,
			def: Definition::ZScript(sema::Datum::Class(sema::Class {
				flags: ClassFlags::NATIVE,
				scope: sema::Scope::Data,
				min_version: None,
			})),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("Thinker")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "Thinker",
			decl: "class Thinker : Object native",
			docs: &[
				r#"A class representing any object in the game that runs logic every game tic, i.e., "thinks." \
				Most classes derive from Thinker, directly or indirectly. \
				The order in which thinkers run is defined by stat numbers."#,
			],
			scope: None,
			def: Definition::ZScript(sema::Datum::Class(sema::Class {
				flags: ClassFlags::NATIVE,
				scope: sema::Scope::Data,
				min_version: None,
			})),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("Actor")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "Actor",
			decl: "class Actor : Thinker native",
			docs: &["A `Thinker` capable of existing within 3D space."],
			scope: None,
			def: Definition::ZScript(sema::Datum::Class(sema::Class {
				flags: ClassFlags::NATIVE,
				scope: sema::Scope::Play,
				min_version: None,
			})),
		},
	);

	Cache {
		class_object: ptr_class_object,
	}
}

// Primitives //////////////////////////////////////////////////////////////////

fn register_primitives(names: &NameInterner, globals: &mut Scope, bump: &mut bumpalo::Bump) {
	// TODO

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("void")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "void",
			decl: "void",
			docs: &[indoc::indoc! {
				"A pseudo-type used to indicate that a function returns no values.

				It can also be used as a function's only (unnamed) parameter, although
				note that this is the same as an empty parameter list:

				```zscript
				void Function(void) {
					return;
				}
				```"
			}],
			scope: None,
			def: Definition::ZScript(sema::Datum::Primitive(sema::Primitive::Bool)),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("bool")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "bool",
			decl: "bool",
			docs: &["A strongly-typed \"truthy\" value, which can only hold a constant `true` or `false`."],
			scope: None,
			def: Definition::ZScript(sema::Datum::Primitive(sema::Primitive::Bool)),
		}
	);

	register_with_members(
		globals,
		bump,
		[
			(
				NsName::Value(names.intern_str("MAX")),
				InternalSymbol {
					lang: LangId::ZScript,
					name: "MAX",
					decl: "int.MAX = 2147483647",
					docs: &["The highest value representable by this type."],
					scope: None,
					def: Definition::ZScript(sema::Datum::Constant),
				},
			),
			(
				NsName::Value(names.intern_str("MIN")),
				InternalSymbol {
					lang: LangId::ZScript,
					name: "MIN",
					decl: "int.MIN = -2147483648",
					docs: &["The lowest value representable by this type."],
					scope: None,
					def: Definition::ZScript(sema::Datum::Constant),
				},
			),
		],
		NsName::Type(names.intern_str("int")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "int",
			decl: "int",
			docs: &["A signed 32-bit integer type."],
			scope: None,
			def: Definition::ZScript(sema::Datum::Primitive(sema::Primitive::Integer {
				bits: 32,
				signed: true,
			})),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("uint")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "uint",
			decl: "uint",
			docs: &["An unsigned 32-bit integer type."],
			scope: None,
			def: Definition::ZScript(sema::Datum::Primitive(sema::Primitive::Integer {
				bits: 32,
				signed: false,
			})),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("float")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "float",
			decl: "float",
			docs: &[indoc::indoc! {
				"A single-precision - i.e. 32-bit - floating-point number.

				Note that when not used as a member variable, `float` is actually
				represented using 64 bits."
			}],
			scope: None,
			def: Definition::ZScript(sema::Datum::Primitive(sema::Primitive::Float)),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("double")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "double",
			decl: "double",
			docs: &[indoc::indoc! {
				"A double-precision - i.e., 64-bit - floating-point number."
			}],
			scope: None,
			def: Definition::ZScript(sema::Datum::Primitive(sema::Primitive::Double)),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("spriteID")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "spriteID",
			decl: "spriteID",
			docs: &["A handle to a sprite."],
			scope: None,
			def: Definition::ZScript(sema::Datum::Primitive(sema::Primitive::SpriteId)),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("statelabel")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "statelabel",
			decl: "statelabel",
			docs: &["A handle to a named place in an actor's state machine."],
			scope: None,
			def: Definition::ZScript(sema::Datum::Primitive(sema::Primitive::StateLabel)),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("voidptr")),
		InternalSymbol {
			lang: LangId::ZScript,
			name: "voidptr",
			decl: "voidptr",
			docs: &["A real memory address without a type. Not available to user code."],
			scope: None,
			def: Definition::ZScript(sema::Datum::Primitive(sema::Primitive::Voidptr)),
		},
	);
}

// Helpers /////////////////////////////////////////////////////////////////////

fn register_with_members(
	globals: &mut Scope,
	bump: &mut bumpalo::Bump,
	members: impl IntoIterator<Item = (NsName, InternalSymbol)>,
	ns_name: NsName,
	mut symbol: InternalSymbol,
) -> SymPtr {
	let mut scope = Scope::default();

	for (m_ns_name, member) in members.into_iter() {
		let ptr = Arena::alloc(bump, Symbol::Internal(member));
		let overwritten = scope.insert(m_ns_name, ptr);
		debug_assert!(overwritten.is_none());
	}

	symbol.scope = Some(scope);
	let ptr = Arena::alloc(bump, Symbol::Internal(symbol));
	let overwritten = globals.insert(ns_name, ptr.clone());
	debug_assert!(overwritten.is_none());

	ptr
}
