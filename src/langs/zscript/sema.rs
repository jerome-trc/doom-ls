//! Infrastructure for semantic representation of ZScript source.

use doomfront::{
	rowan::{ast::AstNode, GreenNode, Language, TextSize},
	zdoom::zscript::{ast, Syn, SyntaxNode},
};
use rustc_hash::FxHashMap;

use crate::{
	names::{IName, StringInterner},
	project::{self, FileId, FilePos, Project, Scope},
};

#[derive(Debug, Default)]
pub(crate) struct Storage {
	pub(super) scopes: FxHashMap<FilePos, Scope>,
}

#[derive(Debug)]
pub(crate) enum Datum {
	Class(ClassDatum),
	Value(ValueDatum),
	Enum(EnumDatum),
	MixinClass(MixinClassDatum),
	Struct(StructDatum),
	Primitive,
}

impl Datum {
	#[must_use]
	pub(crate) fn filepos(&self) -> Option<FilePos> {
		match self {
			Datum::Class(dat_class) => Some(dat_class.filepos),
			Datum::Value(dat_val) => Some(dat_val.filepos),
			Datum::Enum(dat_enum) => Some(dat_enum.filepos),
			Datum::MixinClass(dat_mixin) => Some(dat_mixin.filepos),
			Datum::Struct(dat_struct) => Some(dat_struct.filepos),
			Datum::Primitive => None,
		}
	}

	#[must_use]
	pub(crate) fn name_filepos(&self) -> Option<FilePos> {
		match self {
			Datum::Class(dat_class) => Some(FilePos {
				file: dat_class.filepos.file,
				pos: dat_class.namepos,
			}),
			Datum::Value(dat_val) => Some(FilePos {
				file: dat_val.filepos.file,
				pos: dat_val.namepos,
			}),
			Datum::Enum(dat_enum) => Some(FilePos {
				file: dat_enum.filepos.file,
				pos: dat_enum.namepos,
			}),
			Datum::MixinClass(dat_mixin) => Some(FilePos {
				file: dat_mixin.filepos.file,
				pos: dat_mixin.namepos,
			}),
			Datum::Struct(dat_struct) => Some(FilePos {
				file: dat_struct.filepos.file,
				pos: dat_struct.namepos,
			}),
			Datum::Primitive => None,
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ClassDatum {
	pub(crate) filepos: FilePos,
	pub(crate) namepos: TextSize,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ValueDatum {
	pub(crate) filepos: FilePos,
	pub(crate) namepos: TextSize,
	pub(crate) mutable: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct EnumDatum {
	pub(crate) filepos: FilePos,
	pub(crate) namepos: TextSize,
	pub(crate) variants: FxHashMap<IName, ValueDatum>,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct MixinClassDatum {
	pub(crate) filepos: FilePos,
	pub(crate) namepos: TextSize,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct StructDatum {
	pub(crate) filepos: FilePos,
	pub(crate) namepos: TextSize,
}

#[derive(Debug)]
pub(crate) struct UpdateContext<'a> {
	pub(crate) strings: &'a StringInterner,
	pub(crate) project: &'a mut Project,
	pub(crate) file_id: FileId,
	pub(crate) contributed: Vec<IName>,
}

// Functions ///////////////////////////////////////////////////////////////////

impl UpdateContext<'_> {
	pub(crate) fn update(&mut self, green: GreenNode) {
		debug_assert_eq!(green.kind(), Syn::kind_to_raw(Syn::Root));
		let cursor = SyntaxNode::new_root(green);

		self.project
			.zscript_mut()
			.scopes
			.retain(|filepos, _| filepos.file != self.file_id);

		for child in cursor.children() {
			let Some(top) = ast::TopLevel::cast(child) else { continue; };

			match top {
				ast::TopLevel::ClassDef(classdef) => {
					self.register_class_def(classdef);
				}
				ast::TopLevel::ConstDef(constdef) => {
					let Ok(const_name) = constdef.name() else { continue; };

					let datum = Datum::Value(ValueDatum {
						filepos: FilePos {
							file: self.file_id,
							pos: constdef.syntax().text_range().start(),
						},
						namepos: const_name.text_range().start(),
						mutable: false,
					});

					let iname = self.strings.var_name_nocase(const_name.text());
					self.project
						.globals_mut()
						.insert(iname, project::Datum::ZScript(datum));
					self.contributed.push(iname);
				}
				ast::TopLevel::EnumDef(enumdef) => {
					let Ok(enum_name) = enumdef.name() else { continue; };

					let mut datum = EnumDatum {
						filepos: FilePos {
							file: self.file_id,
							pos: enumdef.syntax().text_range().start(),
						},
						namepos: enum_name.text_range().start(),
						variants: FxHashMap::default(),
					};

					self.register_enum_variants(&mut datum, enumdef);

					let iname = self.strings.type_name_nocase(enum_name.text());
					self.project
						.globals_mut()
						.insert(iname, project::Datum::ZScript(Datum::Enum(datum)));
					self.contributed.push(iname);
				}
				ast::TopLevel::MixinClassDef(mixindef) => {
					let Ok(mixin_name) = mixindef.name() else { continue; };

					let datum = Datum::MixinClass(MixinClassDatum {
						filepos: FilePos {
							file: self.file_id,
							pos: mixindef.syntax().text_range().start(),
						},
						namepos: mixin_name.text_range().start(),
					});

					let iname = self.strings.type_name_nocase(mixin_name.text());
					self.project
						.globals_mut()
						.insert(iname, project::Datum::ZScript(datum));
					self.contributed.push(iname);
				}
				ast::TopLevel::StructDef(structdef) => {
					let Ok(struct_name) = structdef.name() else { continue; };

					let datum = Datum::Struct(StructDatum {
						filepos: FilePos {
							file: self.file_id,
							pos: structdef.syntax().text_range().start(),
						},
						namepos: struct_name.text_range().start(),
					});

					let iname = self.strings.type_name_nocase(struct_name.text());
					self.project
						.globals_mut()
						.insert(iname, project::Datum::ZScript(datum));
					self.contributed.push(iname);
				}
				ast::TopLevel::ClassExtend(_)
				| ast::TopLevel::StructExtend(_)
				| ast::TopLevel::Include(_)
				| ast::TopLevel::Version(_) => continue, // TODO
			}
		}
	}

	fn register_class_def(&mut self, classdef: ast::ClassDef) {
		let Ok(class_name) = classdef.name() else { return; };

		let filepos = FilePos {
			file: self.file_id,
			pos: classdef.syntax().text_range().start(),
		};

		let datum = Datum::Class(ClassDatum {
			filepos,
			namepos: class_name.text_range().start(),
		});

		let iname = self.strings.type_name_nocase(class_name.text());
		self.project
			.globals_mut()
			.insert(iname, project::Datum::ZScript(datum));
		self.contributed.push(iname);

		let mut scope = Scope::default();

		for innard in classdef.innards() {
			match innard {
				ast::ClassInnard::Const(constdef) => {
					let Ok(const_name) = constdef.name() else { continue; };
					let iname = self.strings.get_or_intern_nocase(const_name.text());

					scope.insert(
						IName::Variable(iname),
						project::Datum::ZScript(Datum::Value(ValueDatum {
							filepos: FilePos {
								file: self.file_id,
								pos: constdef.syntax().text_range().start(),
							},
							namepos: const_name.text_range().start(),
							mutable: false,
						})),
					);
				}
				ast::ClassInnard::Enum(enumdef) => {
					let Ok(enum_name) = enumdef.name() else { continue; };
					let iname = self.strings.get_or_intern_nocase(enum_name.text());

					let mut dat_enum = EnumDatum {
						filepos: FilePos {
							file: self.file_id,
							pos: enumdef.syntax().text_range().start(),
						},
						namepos: enum_name.text_range().start(),
						variants: FxHashMap::default(),
					};

					self.register_enum_variants(&mut dat_enum, enumdef);

					scope.insert(
						IName::Type(iname),
						project::Datum::ZScript(Datum::Enum(dat_enum)),
					);
				}
				_ => {} // TODO
			}
		}

		if let Some(ancestor) = classdef.parent_class() {
			let iname = self.strings.type_name_nocase(ancestor.text());
			scope.addenda.push(iname);
		}

		self.project.zscript_mut().scopes.insert(filepos, scope);
	}

	fn register_enum_variants(&mut self, datum: &mut EnumDatum, enumdef: ast::EnumDef) {
		for variant in enumdef.variants() {
			let name = self.strings.var_name_nocase(variant.name().text());

			datum.variants.insert(
				name,
				ValueDatum {
					filepos: FilePos {
						file: self.file_id,
						pos: variant.syntax().text_range().start(),
					},
					namepos: variant.syntax().text_range().start(),
					mutable: false,
				},
			);
		}
	}
}

#[must_use]
pub(crate) fn native_symbols() -> [(&'static str, Datum); 6] {
	[
		("color", Datum::Primitive),
		("sound", Datum::Primitive),
		("spriteid", Datum::Primitive),
		("statelabel", Datum::Primitive),
		("textureid", Datum::Primitive),
		("voidptr", Datum::Primitive),
	]
}
