//! Infrastructure for semantic representation of ZScript source.

use std::rc::Rc;

use doomfront::{
	rowan::{ast::AstNode, GreenNode, Language, TextRange},
	zdoom::zscript::{ast, Syn, SyntaxNode, SyntaxToken},
};
use petgraph::{
	graph::{DefaultIx, NodeIndex},
	prelude::DiGraph,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
	names::{IName, StringInterner},
	paths::FileId,
	project::{self, DatumPos, Project, Scope, StackedScope},
};

#[derive(Debug)]
pub(crate) enum Datum {
	Class(ClassDatum),
	Value(ValueDatum),
	Enum(EnumDatum),
	MixinClass(MixinClassDatum),
	Struct(StructDatum),
	Function(FunctionDatum),
	Primitive(PrimitiveDatum),
}

impl Datum {
	#[must_use]
	pub(crate) fn pos(&self) -> Option<DatumPos> {
		match self {
			Self::Class(dat_class) => dat_class.position,
			Self::Value(dat_val) => dat_val.position,
			Self::Enum(dat_enum) => dat_enum.position,
			Self::MixinClass(dat_mixin) => dat_mixin.position,
			Self::Struct(dat_struct) => dat_struct.position,
			Self::Function(dat_fn) => dat_fn.position,
			Self::Primitive(_) => None,
		}
	}

	pub(crate) fn add_scopes_containing(&self, scopes: &mut Vec<StackedScope>, range: TextRange) {
		match self {
			Self::Class(dat_class) => {
				scopes.push(StackedScope {
					ix_project: scopes.last().unwrap().ix_project,
					inner: dat_class.scope.clone(),
					is_addendum: false,
				});

				for d in dat_class.scope.values() {
					if d.pos()
						.is_some_and(|dpos| dpos.full_range.contains_range(range))
					{
						d.add_scopes_containing(scopes, range);
					}
				}
			}
			Self::Struct(dat_struct) => {
				scopes.push(StackedScope {
					ix_project: scopes.last().unwrap().ix_project,
					inner: dat_struct.scope.clone(),
					is_addendum: false,
				});

				for d in dat_struct.scope.values() {
					if d.pos()
						.is_some_and(|dpos| dpos.full_range.contains_range(range))
					{
						d.add_scopes_containing(scopes, range);
					}
				}
			}
			Self::Function(dat_fn) => {
				dat_fn.add_scopes_containing(scopes, range);
			}
			Self::MixinClass(_) => {
				// TODO
			}
			Self::Value(_) | Self::Enum(_) | Self::Primitive(_) => {}
		}
	}
}

#[derive(Debug)]
pub(crate) struct ClassDatum {
	pub(crate) name: IName,
	pub(crate) position: Option<DatumPos>,
	pub(crate) scope: Rc<Scope>,
	pub(crate) parent: Option<IName>,
}

#[derive(Debug)]
pub(crate) struct ValueDatum {
	pub(crate) name: IName,
	pub(crate) position: Option<DatumPos>,
	pub(crate) mutable: bool,
}

#[derive(Debug)]
pub(crate) struct EnumDatum {
	pub(crate) name: IName,
	pub(crate) position: Option<DatumPos>,
	pub(crate) variants: FxHashSet<IName>,
}

#[derive(Debug)]
pub(crate) struct MixinClassDatum {
	pub(crate) name: IName,
	pub(crate) position: Option<DatumPos>,
	pub(crate) _scope: Rc<Scope>,
}

#[derive(Debug)]
pub(crate) struct StructDatum {
	pub(crate) name: IName,
	pub(crate) position: Option<DatumPos>,
	pub(crate) scope: Rc<Scope>,
}

#[derive(Debug)]
pub(crate) struct FunctionDatum {
	pub(crate) name: IName,
	pub(crate) position: Option<DatumPos>,
	/// The first node is always the entry block.
	pub(crate) cflow: DiGraph<Rc<Scope>, ()>,
	pub(crate) block_map: FxHashMap<TextRange, NodeIndex<DefaultIx>>,
}

#[derive(Debug)]
pub(crate) struct PrimitiveDatum {
	pub(crate) name: IName,
}

impl FunctionDatum {
	pub(crate) fn add_scopes_containing(&self, scopes: &mut Vec<StackedScope>, range: TextRange) {
		let Some(i) = self.block_map.iter().find_map(|kvp| {
			kvp.0.contains_range(range).then_some(*kvp.1)
		}) else {
			return;
		};

		let scope = &self.cflow[i];

		for datum in scope.values() {
			if datum
				.pos()
				.is_some_and(|dpos| dpos.full_range.contains_range(range))
			{
				datum.add_scopes_containing(scopes, range);
			}
		}
	}
}

#[derive(Debug)]
pub(crate) struct UpdateContext<'a> {
	pub(crate) strings: &'a StringInterner,
	pub(crate) project: &'a mut Project,
	pub(crate) file_id: FileId,
	pub(crate) contributed: Vec<IName>,
}

// Update functionality ////////////////////////////////////////////////////////

impl UpdateContext<'_> {
	pub(crate) fn update(&mut self, green: GreenNode) {
		debug_assert_eq!(green.kind(), Syn::kind_to_raw(Syn::Root));
		let cursor = SyntaxNode::new_root(green);

		for child in cursor.children() {
			let Some(top) = ast::TopLevel::cast(child) else { continue; };

			match top {
				ast::TopLevel::ClassDef(classdef) => {
					self.register_class_def(classdef);
				}
				ast::TopLevel::ConstDef(constdef) => {
					let Ok(const_name) = constdef.name() else { continue; };
					let iname = self.strings.value_name_nocase(const_name.text());

					let datum = Datum::Value(ValueDatum {
						name: iname,
						position: Some(DatumPos {
							file: self.file_id,
							full_range: constdef.syntax().text_range(),
							name_range: const_name.text_range(),
						}),
						mutable: false,
					});

					self.project
						.globals_mut()
						.insert(iname, project::Datum::ZScript(datum));

					self.contributed.push(iname);
				}
				ast::TopLevel::EnumDef(enumdef) => {
					let Ok(enum_name) = enumdef.name() else { continue; };
					let iname = self.strings.type_name_nocase(enum_name.text());

					let mut datum = EnumDatum {
						name: iname,
						position: Some(DatumPos {
							file: self.file_id,
							full_range: enumdef.syntax().text_range(),
							name_range: enum_name.text_range(),
						}),
						variants: FxHashSet::default(),
					};

					self.register_enum_variants(&mut datum, enumdef);

					self.project
						.globals_mut()
						.insert(iname, project::Datum::ZScript(Datum::Enum(datum)));

					self.contributed.push(iname);
				}
				ast::TopLevel::MixinClassDef(mixindef) => {
					let Ok(mixin_name) = mixindef.name() else { continue; };
					let iname = self.strings.type_name_nocase(mixin_name.text());

					let datum = Datum::MixinClass(MixinClassDatum {
						name: iname,
						position: Some(DatumPos {
							file: self.file_id,
							full_range: mixindef.syntax().text_range(),
							name_range: mixin_name.text_range(),
						}),
						_scope: Rc::<Scope>::default(),
					});

					self.project
						.globals_mut()
						.insert(iname, project::Datum::ZScript(datum));

					self.contributed.push(iname);
				}
				ast::TopLevel::StructDef(structdef) => {
					let Ok(struct_name) = structdef.name() else { continue; };
					let iname = self.strings.type_name_nocase(struct_name.text());

					let datum = Datum::Struct(StructDatum {
						name: iname,
						position: Some(DatumPos {
							file: self.file_id,
							full_range: structdef.syntax().text_range(),
							name_range: struct_name.text_range(),
						}),
						scope: Rc::<Scope>::default(),
					});

					self.project
						.globals_mut()
						.insert(iname, project::Datum::ZScript(datum));

					self.contributed.push(iname);
				}
				// TODO: Second pass?
				ast::TopLevel::ClassExtend(_) | ast::TopLevel::StructExtend(_) => continue,
				ast::TopLevel::Include(_) | ast::TopLevel::Version(_) => continue,
			}
		}
	}

	fn register_class_def(&mut self, classdef: ast::ClassDef) {
		let Ok(class_name) = classdef.name() else { return; };
		let iname = self.strings.type_name_nocase(class_name.text());

		let mut datum = ClassDatum {
			name: iname,
			position: Some(DatumPos {
				file: self.file_id,
				full_range: classdef.syntax().text_range(),
				name_range: class_name.text_range(),
			}),
			scope: Rc::<Scope>::default(),
			parent: Some(
				classdef
					.parent_class()
					.map(|ancestor| self.strings.type_name_nocase(ancestor.text()))
					.unwrap_or_else(|| self.strings.type_name_nocase("Object")),
			),
		};

		let mut scope = Scope::default();

		for innard in classdef.innards() {
			match innard {
				ast::ClassInnard::Const(constdef) => {
					let Ok(const_name) = constdef.name() else { continue; };
					let iname = self.strings.value_name_nocase(const_name.text());

					scope.insert(
						iname,
						project::Datum::ZScript(Datum::Value(ValueDatum {
							name: iname,
							position: Some(DatumPos {
								file: self.file_id,
								full_range: constdef.syntax().text_range(),
								name_range: const_name.text_range(),
							}),
							mutable: false,
						})),
					);
				}
				ast::ClassInnard::Enum(enumdef) => {
					let Ok(enum_name) = enumdef.name() else { continue; };
					let iname = self.strings.type_name_nocase(enum_name.text());

					let mut dat_enum = EnumDatum {
						name: iname,
						position: Some(DatumPos {
							file: self.file_id,
							full_range: enumdef.syntax().text_range(),
							name_range: enum_name.text_range(),
						}),
						variants: FxHashSet::default(),
					};

					self.register_enum_variants(&mut dat_enum, enumdef);

					scope.insert(iname, project::Datum::ZScript(Datum::Enum(dat_enum)));
				}
				ast::ClassInnard::Function(fndecl) => {
					let iname = self.strings.value_name_nocase(fndecl.name().text());

					let dat_fn = FunctionDatum {
						name: iname,
						position: Some(DatumPos {
							file: self.file_id,
							full_range: fndecl.syntax().text_range(),
							name_range: fndecl.name().text_range(),
						}),
						cflow: DiGraph::default(),
						block_map: FxHashMap::default(),
					};

					scope.insert(iname, project::Datum::ZScript(Datum::Function(dat_fn)));
				}
				_ => {} // TODO
			}
		}

		datum.scope = Rc::new(scope);

		self.project
			.globals_mut()
			.insert(iname, project::Datum::ZScript(Datum::Class(datum)));

		self.contributed.push(iname);
	}

	fn register_enum_variants(&mut self, datum: &mut EnumDatum, enumdef: ast::EnumDef) {
		for variant in enumdef.variants() {
			let iname = self.strings.value_name_nocase(variant.name().text());
			datum.variants.insert(iname);
		}
	}
}

// Syntax introspection ////////////////////////////////////////////////////////

/// Wherever the token is in the tree, this will retrieve a definition for a
/// class, struct, enum, symbolic constant, or mixin class which is a child
/// of the syntax tree's root node.
#[must_use]
pub(crate) fn global_containing(token: &SyntaxToken) -> Option<ast::TopLevel> {
	let mut parent = token.parent();

	while let Some(p) = parent {
		if p.parent().is_some_and(|gp| gp.kind() == Syn::Root) {
			if let Some(t) = ast::TopLevel::cast(p.clone()) {
				return Some(t);
			}
		}

		parent = p.parent();
	}

	None
}

#[must_use]
pub(crate) fn top_level_name(top: ast::TopLevel) -> Option<SyntaxToken> {
	match top {
		ast::TopLevel::ClassDef(classdef) => classdef.name().ok(),
		ast::TopLevel::ClassExtend(classext) => classext.name().ok(),
		ast::TopLevel::ConstDef(constdef) => constdef.name().ok(),
		ast::TopLevel::EnumDef(enumdef) => enumdef.name().ok(),
		ast::TopLevel::MixinClassDef(mixindef) => mixindef.name().ok(),
		ast::TopLevel::StructDef(structdef) => structdef.name().ok(),
		ast::TopLevel::StructExtend(structext) => structext.name().ok(),
		ast::TopLevel::Version(_) | ast::TopLevel::Include(_) => None,
	}
}

// Native symbols //////////////////////////////////////////////////////////////

#[must_use]
pub(crate) fn native_symbols(core: &crate::Core) -> [(IName, Datum); 13] {
	// TODO: Would be good if this could be sourced from TOML files kept in
	// a repo under the https://github.com/zdoom-docs organization, and
	// integrated using a build script.
	[
		{
			let iname = core.strings.type_name_nocase("array");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("color");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("map");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("mapiterator");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("sound");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("spriteid");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("statelabel");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("textureid");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("vector2");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("vector3");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		{
			let iname = core.strings.type_name_nocase("voidptr");
			(iname, Datum::Primitive(PrimitiveDatum { name: iname }))
		},
		// Classes /////////////////////////////////////////////////////////////
		{
			let iname = core.strings.type_name_nocase("Object");

			(
				iname,
				Datum::Class(ClassDatum {
					name: iname,
					position: None,
					scope: Rc::new(Scope::default()),
					parent: None,
				}),
			)
		},
		// Enumerations ////////////////////////////////////////////////////////
		{
			let iname = core.strings.type_name_nocase("EGameState");

			(
				iname,
				Datum::Enum(EnumDatum {
					name: iname,
					position: None,
					variants: {
						let mut set = FxHashSet::default();

						for name in [
							core.strings.value_name_nocase("GS_LEVEL"),
							core.strings.value_name_nocase("GS_INTERMISSION"),
							core.strings.value_name_nocase("GS_FINALE"),
							core.strings.value_name_nocase("GS_DEMOSCREEN"),
							core.strings.value_name_nocase("GS_FULLCONSOLE"),
							core.strings.value_name_nocase("GS_HIDECONSOLE"),
							core.strings.value_name_nocase("GS_STARTUP"),
							core.strings.value_name_nocase("GS_TITLELEVEL"),
							core.strings.value_name_nocase("GS_INTRO"),
							core.strings.value_name_nocase("GS_CUTSCENE"),
							core.strings.value_name_nocase("GS_MENUSCREEN"),
						] {
							set.insert(name);
						}

						set
					},
				}),
			)
		},
	]
}
