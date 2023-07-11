//! Infrastructure for semantic representation of ZScript source.

use doomfront::{
	rowan::{ast::AstNode, GreenNode, Language},
	zdoom::zscript::{ast, Syn, SyntaxNode},
};
use rustc_hash::FxHashMap;
use slotmap::SlotMap;

use crate::{
	names::{Name, StringInterner},
	project::{FileId, FilePos, SymbolDelta},
};

#[derive(Debug, Default)]
pub(crate) struct Storage {
	pub(super) data: SlotMap<DatumKey, Datum>,
}

slotmap::new_key_type! { pub(crate) struct DatumKey; }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SymbolKey {
	Class(DatumKey),
	Constant(DatumKey),
	Enum(DatumKey),
	MixinClass(DatumKey),
	Struct(DatumKey),
}

impl SymbolKey {
	#[must_use]
	pub fn into_inner(self) -> DatumKey {
		match self {
			SymbolKey::Class(k)
			| SymbolKey::Constant(k)
			| SymbolKey::Enum(k)
			| SymbolKey::MixinClass(k)
			| SymbolKey::Struct(k) => k,
		}
	}
}

#[derive(Debug)]
pub(crate) enum Datum {
	Class(ClassDatum),
	Constant(ConstantDatum),
	Enum(EnumDatum),
	MixinClass(MixinClassDatum),
	Struct(StructDatum),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ClassDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ConstantDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct EnumDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
	pub(crate) variants: FxHashMap<Name, ConstantDatum>,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct MixinClassDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct StructDatum {
	/// `filepos.pos` corresponds to the first character of the identifier.
	pub(crate) filepos: FilePos,
}

#[derive(Debug)]
pub(crate) struct UpdateContext<'a> {
	pub(crate) strings: &'a StringInterner,
	pub(crate) storage: &'a mut Storage,
	pub(crate) delta: &'a mut SymbolDelta<SymbolKey>,
	pub(crate) file_id: FileId,
}

// Functions ///////////////////////////////////////////////////////////////////

pub(crate) fn update(mut ctx: UpdateContext, green: GreenNode) {
	debug_assert_eq!(green.kind(), Syn::kind_to_raw(Syn::Root));
	let cursor = SyntaxNode::new_root(green);

	for removed in &ctx.delta.removed {
		ctx.storage.data.remove(removed.1.into_inner());
	}

	for child in cursor.children() {
		let Some(top) = ast::TopLevel::cast(child) else { continue; };

		match top {
			ast::TopLevel::ClassDef(classdef) => {
				let Ok(class_name) = classdef.name() else { continue; };

				let dat_k = ctx.storage.data.insert(Datum::Class(ClassDatum {
					filepos: FilePos {
						file: ctx.file_id,
						pos: class_name.text_range().start(),
					},
				}));

				let name = ctx.strings.type_name_nocase(class_name.text());
				ctx.delta.added.push((name, SymbolKey::Class(dat_k)));
			}
			ast::TopLevel::ConstDef(constdef) => {
				let Ok(const_name) = constdef.name() else { continue; };

				let dat_k = ctx.storage.data.insert(Datum::Constant(ConstantDatum {
					filepos: FilePos {
						file: ctx.file_id,
						pos: const_name.text_range().start(),
					},
				}));

				let name = ctx.strings.var_name_nocase(const_name.text());
				ctx.delta.added.push((name, SymbolKey::Constant(dat_k)));
			}
			ast::TopLevel::EnumDef(enumdef) => {
				let Ok(enum_name) = enumdef.name() else { continue; };

				let mut datum = EnumDatum {
					filepos: FilePos {
						file: ctx.file_id,
						pos: enum_name.text_range().start(),
					},
					variants: FxHashMap::default(),
				};

				declare_enum_variants(&mut ctx, &mut datum, enumdef);

				let dat_k = ctx.storage.data.insert(Datum::Enum(datum));

				let name = ctx.strings.type_name_nocase(enum_name.text());
				ctx.delta.added.push((name, SymbolKey::Enum(dat_k)));
			}
			ast::TopLevel::MixinClassDef(mixindef) => {
				let Ok(mixin_name) = mixindef.name() else { continue; };

				let dat_k = ctx.storage.data.insert(Datum::MixinClass(MixinClassDatum {
					filepos: FilePos {
						file: ctx.file_id,
						pos: mixin_name.text_range().start(),
					},
				}));

				let name = ctx.strings.type_name_nocase(mixin_name.text());
				ctx.delta.added.push((name, SymbolKey::MixinClass(dat_k)));
			}
			ast::TopLevel::StructDef(structdef) => {
				let Ok(struct_name) = structdef.name() else { continue; };

				let dat_k = ctx.storage.data.insert(Datum::Struct(StructDatum {
					filepos: FilePos {
						file: ctx.file_id,
						pos: struct_name.text_range().start(),
					},
				}));

				let name = ctx.strings.type_name_nocase(struct_name.text());
				ctx.delta.added.push((name, SymbolKey::Struct(dat_k)));
			}
			ast::TopLevel::ClassExtend(_)
			| ast::TopLevel::StructExtend(_)
			| ast::TopLevel::Include(_)
			| ast::TopLevel::Version(_) => continue,
		}
	}
}

fn declare_enum_variants(ctx: &mut UpdateContext, datum: &mut EnumDatum, enumdef: ast::EnumDef) {
	for variant in enumdef.variants() {
		let name = ctx.strings.var_name_nocase(variant.name().text());

		datum.variants.insert(
			name,
			ConstantDatum {
				filepos: FilePos {
					file: ctx.file_id,
					pos: variant.syntax().text_range().start(),
				},
			},
		);
	}
}
