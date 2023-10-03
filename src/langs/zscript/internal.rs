//! ZScript internal symbol registration.
//!
//! Even if it were good UX to expect users to unzip their gzdoom.pk3 and point
//! the server's configuration to it, it would not account for compiler intrinsics
//! and primitives and so on. All those symbols are instead emitted from here.

use doomfront::zdoom;

use crate::{
	arena::Arena,
	core::Scope,
	data::{Definition, InternalSymbol, SymPtr, Symbol},
	intern::{NameInterner, NsName},
	langs::LangId,
};

use super::sema::{self, FunctionFlags};

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
	let ptr_class_object = register_with_members(
		globals,
		bump,
		[
			(
				NsName::Value(names.intern_str("TICRATE")),
				InternalSymbol {
					lang: LangId::ZScript,
					decl: "const Object.TICRATE = 35",
					docs: &[
						r#"The "simulation rate" of the game; that is, the number of updates or "ticks" in a second."#,
					],
					scope: Scope::default(),
					def: Definition::ZScript(sema::Datum::Constant),
				},
			),
			(
				NsName::Value(names.intern_str("GetClass")),
				InternalSymbol {
					lang: LangId::ZScript,
					decl: "virtualScope class<Object> Object.GetClass()",
					docs: &["Returns the class type of this object."],
					scope: Scope::default(),
					def: Definition::ZScript(sema::Datum::Function(sema::Function {
						scope: sema::Scope::Virtual,
						flags: FunctionFlags::empty(),
						vis: sema::Visibility::Public,
						_min_version: zdoom::Version::V1_0_0,
						deprecated: None,
					})),
				},
			),
		],
		NsName::Type(names.intern_str("Object")),
		InternalSymbol {
			lang: LangId::ZScript,
			decl: "class Object native",
			docs: &["The base class of all classes except itself."],
			scope: Scope::default(),
			def: Definition::ZScript(sema::Datum::Class),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("Thinker")),
		InternalSymbol {
			lang: LangId::ZScript,
			decl: "class Thinker : Object native",
			docs: &[
				r#"A class representing any object in the game that runs logic every game tic, i.e., "thinks." \
				Most classes derive from Thinker, directly or indirectly. \
				The order in which thinkers run is defined by stat numbers."#,
			],
			scope: Scope::default(),
			def: Definition::ZScript(sema::Datum::Class),
		},
	);

	register_with_members(
		globals,
		bump,
		[],
		NsName::Type(names.intern_str("Actor")),
		InternalSymbol {
			lang: LangId::ZScript,
			decl: "class Actor : Thinker native",
			docs: &["A `Thinker` capable of existing within 3D space."],
			scope: Scope::default(),
			def: Definition::ZScript(sema::Datum::Class),
		},
	);

	Cache {
		class_object: ptr_class_object,
	}
}

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
		scope.insert(m_ns_name, ptr);
	}

	symbol.scope = scope;
	let ptr = Arena::alloc(bump, Symbol::Internal(symbol));
	globals.insert(ns_name, ptr.clone());

	ptr
}
