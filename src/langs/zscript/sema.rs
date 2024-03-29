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
	project::{self, DatumPos, ParsedFile, Project, Scope, ScopeStack, StackedScope},
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
			Self::Class(dat_class) => dat_class.position(),
			Self::Value(dat_val) => dat_val.position(),
			Self::Enum(dat_enum) => dat_enum.position(),
			Self::MixinClass(dat_mixin) => dat_mixin.position(),
			Self::Struct(dat_struct) => dat_struct.position(),
			Self::Function(dat_fn) => dat_fn.position(),
			Self::Primitive(_) => None,
		}
	}

	pub(crate) fn add_scopes_containing(&self, scopes: &mut ScopeStack, range: TextRange) {
		match self {
			Self::Class(dat_class) => {
				let project_ix = scopes.last().unwrap().project_ix;

				scopes.push(StackedScope {
					project_ix,
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
				let project_ix = scopes.last().unwrap().project_ix;

				scopes.push(StackedScope {
					project_ix,
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
	pub(crate) source: ClassSource,
	pub(crate) scope: Rc<Scope>,
	/// Only `None` for the native `Object` class.
	pub(crate) parent: Option<IName>,
}

#[derive(Debug)]
pub(crate) enum ClassSource {
	User {
		position: DatumPos,
		ast: ast::ClassDef,
	},
	Native {
		decl: &'static str,
		doc: &'static str,
	},
}

impl ClassDatum {
	#[must_use]
	pub(crate) fn position(&self) -> Option<DatumPos> {
		if let ClassSource::User { position, .. } = &self.source {
			Some(*position)
		} else {
			None
		}
	}
}

#[derive(Debug)]
pub(crate) struct ValueDatum {
	pub(crate) name: IName,
	pub(crate) source: ValueSource,
	pub(crate) kind: ValueKind,
}

#[derive(Debug)]
pub(crate) enum ValueSource {
	User {
		position: DatumPos,
		ast: SyntaxNode,
	},
	Native {
		decl: &'static str,
		doc: &'static str,
	},
}

impl ValueDatum {
	#[must_use]
	pub(crate) fn position(&self) -> Option<DatumPos> {
		if let ValueSource::User { position, .. } = &self.source {
			Some(*position)
		} else {
			None
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ValueKind {
	/// [`ValueSource::User::ast`] can always be cast to an [`ast::LocalVar`].
	///
	/// [`ValueSource::User::ast`]: ValueSource::User
	_Local,
	/// [`ValueSource::User::ast`] can always be cast to an [`ast::FieldDecl`].
	///
	/// [`ValueSource::User::ast`]: ValueSource::User
	Field,
	/// [`ValueSource::User::ast`] can always be cast to an [`ast::ConstDef`].
	///
	/// [`ValueSource::User::ast`]: ValueSource::User
	Constant,
	/// [`ValueSource::User::ast`] can always be cast to an [`ast::EnumVariant`].
	///
	/// [`ValueSource::User::ast`]: ValueSource::User
	EnumVariant,
}

#[derive(Debug)]
pub(crate) struct EnumDatum {
	pub(crate) name: IName,
	pub(crate) underlying: ast::EnumType,
	pub(crate) source: EnumSource,
	pub(crate) variants: FxHashSet<IName>,
}

#[derive(Debug)]
pub(crate) enum EnumSource {
	User {
		position: DatumPos,
		ast: ast::EnumDef,
	},
	Native {
		decl: &'static str,
		doc: &'static str,
	},
}

impl EnumDatum {
	#[must_use]
	pub(crate) fn position(&self) -> Option<DatumPos> {
		if let EnumSource::User { position, .. } = &self.source {
			Some(*position)
		} else {
			None
		}
	}
}

#[derive(Debug)]
pub(crate) struct MixinClassDatum {
	pub(crate) name: IName,
	pub(crate) source: MixinClassSource,
	pub(crate) _scope: Rc<Scope>,
}

#[derive(Debug)]
pub(crate) enum MixinClassSource {
	User {
		position: DatumPos,
		ast: ast::MixinClassDef,
	},
	/// Note that no native mixins currently exist.
	#[allow(dead_code)]
	Native {
		decl: &'static str,
		doc: &'static str,
	},
}

impl MixinClassDatum {
	#[must_use]
	pub(crate) fn position(&self) -> Option<DatumPos> {
		if let MixinClassSource::User { position, .. } = &self.source {
			Some(*position)
		} else {
			None
		}
	}
}

#[derive(Debug)]
pub(crate) struct StructDatum {
	pub(crate) name: IName,
	pub(crate) source: StructSource,
	pub(crate) scope: Rc<Scope>,
}

#[derive(Debug)]
pub(crate) enum StructSource {
	User {
		position: DatumPos,
		ast: ast::StructDef,
	},
	Native {
		decl: &'static str,
		doc: &'static str,
	},
}

impl StructDatum {
	#[must_use]
	pub(crate) fn position(&self) -> Option<DatumPos> {
		if let StructSource::User { position, .. } = &self.source {
			Some(*position)
		} else {
			None
		}
	}
}

#[derive(Debug)]
pub(crate) struct FunctionDatum {
	pub(crate) name: IName,
	pub(crate) source: FunctionSource,
	pub(crate) is_const: bool,
	#[allow(unused)]
	pub(crate) is_static: bool,
	pub(crate) body: Option<FunctionBody>,
}

#[derive(Debug)]
pub(crate) enum FunctionSource {
	User {
		position: DatumPos,
		ast: ast::FunctionDecl,
	},
	Native {
		signature: &'static str,
		doc: &'static str,
	},
}

#[derive(Debug)]
pub(crate) struct FunctionBody {
	/// The first node is always the entry block.
	pub(crate) cflow: DiGraph<Rc<Scope>, ()>,
	pub(crate) block_map: FxHashMap<TextRange, NodeIndex<DefaultIx>>,
}

impl FunctionDatum {
	#[must_use]
	pub(crate) fn position(&self) -> Option<DatumPos> {
		if let FunctionSource::User { position, .. } = &self.source {
			Some(*position)
		} else {
			None
		}
	}
}

#[derive(Debug)]
pub(crate) struct PrimitiveDatum {
	pub(crate) name: IName,
	pub(crate) doc: &'static str,
	pub(crate) _scope: Rc<Scope>,
}

impl FunctionDatum {
	pub(crate) fn add_scopes_containing(&self, scopes: &mut ScopeStack, range: TextRange) {
		let Some(body) = &self.body else { return; };

		let Some(i) = body.block_map.iter().find_map(|kvp| {
			kvp.0.contains_range(range).then_some(*kvp.1)
		}) else {
			return;
		};

		let scope = &body.cflow[i];

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
pub(crate) struct UpdateContext<'c> {
	pub(crate) strings: &'c StringInterner,
	pub(crate) project: &'c mut Project,
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
					self.register_const_def(constdef);
				}
				ast::TopLevel::EnumDef(enumdef) => {
					let Ok(enum_name) = enumdef.name() else { continue; };
					let iname = self.strings.type_name_nocase(enum_name.text());

					let mut datum = EnumDatum {
						name: iname,
						source: EnumSource::User {
							position: DatumPos {
								file: self.file_id,
								full_range: enumdef.syntax().text_range(),
								name_range: enum_name.text_range(),
							},
							ast: enumdef.clone(),
						},
						underlying: enumdef
							.type_spec()
							.map(|tspec| tspec.1)
							.unwrap_or(ast::EnumType::KwInt),
						variants: FxHashSet::default(),
					};

					self.register_enum_variants(&mut datum, enumdef, None);
					self.project
						.globals_mut()
						.insert(iname, Rc::new(project::Datum::ZScript(Datum::Enum(datum))));

					self.contributed.push(iname);
				}
				ast::TopLevel::MixinClassDef(mixindef) => {
					let Ok(mixin_name) = mixindef.name() else { continue; };
					let iname = self.strings.type_name_nocase(mixin_name.text());

					let datum = Datum::MixinClass(MixinClassDatum {
						name: iname,
						source: MixinClassSource::User {
							position: DatumPos {
								file: self.file_id,
								full_range: mixindef.syntax().text_range(),
								name_range: mixin_name.text_range(),
							},
							ast: mixindef,
						},
						_scope: Rc::<Scope>::default(),
					});

					self.project
						.globals_mut()
						.insert(iname, Rc::new(project::Datum::ZScript(datum)));

					self.contributed.push(iname);
				}
				ast::TopLevel::StructDef(structdef) => {
					let Ok(struct_name) = structdef.name() else { continue; };
					let iname = self.strings.type_name_nocase(struct_name.text());

					let datum = Datum::Struct(StructDatum {
						name: iname,
						source: StructSource::User {
							position: DatumPos {
								file: self.file_id,
								full_range: structdef.syntax().text_range(),
								name_range: struct_name.text_range(),
							},
							ast: structdef,
						},
						scope: Rc::<Scope>::default(),
					});

					self.project
						.globals_mut()
						.insert(iname, Rc::new(project::Datum::ZScript(datum)));

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
			source: ClassSource::User {
				position: DatumPos {
					file: self.file_id,
					full_range: classdef.syntax().text_range(),
					name_range: class_name.text_range(),
				},
				ast: classdef.clone(),
			},
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
						Rc::new(project::Datum::ZScript(Datum::Value(ValueDatum {
							name: iname,
							source: ValueSource::User {
								position: DatumPos {
									file: self.file_id,
									full_range: constdef.syntax().text_range(),
									name_range: const_name.text_range(),
								},
								ast: constdef.syntax().clone(),
							},
							kind: ValueKind::Constant,
						}))),
					);
				}
				ast::ClassInnard::Enum(enumdef) => {
					let Ok(enum_name) = enumdef.name() else { continue; };
					let iname = self.strings.type_name_nocase(enum_name.text());

					let mut dat_enum = EnumDatum {
						name: iname,
						source: EnumSource::User {
							position: DatumPos {
								file: self.file_id,
								full_range: enumdef.syntax().text_range(),
								name_range: enum_name.text_range(),
							},
							ast: enumdef.clone(),
						},
						underlying: enumdef
							.type_spec()
							.map(|tspec| tspec.1)
							.unwrap_or(ast::EnumType::KwInt),
						variants: FxHashSet::default(),
					};

					self.register_enum_variants(&mut dat_enum, enumdef, Some(&mut scope));

					scope.insert(
						iname,
						Rc::new(project::Datum::ZScript(Datum::Enum(dat_enum))),
					);
				}
				ast::ClassInnard::Function(fndecl) => {
					self.register_function_decl(fndecl, &mut scope);
				}
				ast::ClassInnard::Field(field) => {
					for field_name in field.names() {
						let ident = field_name.ident();
						let iname = self.strings.value_name_nocase(ident.text());

						scope.insert(
							iname,
							Rc::new(project::Datum::ZScript(Datum::Value(ValueDatum {
								name: iname,
								source: ValueSource::User {
									position: DatumPos {
										file: self.file_id,
										full_range: field.syntax().text_range(),
										name_range: ident.text_range(),
									},
									ast: field.syntax().clone(),
								},
								kind: ValueKind::Field,
							}))),
						);
					}
				}
				_ => {} // TODO
			}
		}

		datum.scope = Rc::new(scope);

		self.project
			.globals_mut()
			.insert(iname, Rc::new(project::Datum::ZScript(Datum::Class(datum))));

		self.contributed.push(iname);
	}

	fn register_const_def(&mut self, constdef: ast::ConstDef) {
		let Ok(const_name) = constdef.name() else { return; };
		let iname = self.strings.value_name_nocase(const_name.text());

		let datum = Datum::Value(ValueDatum {
			name: iname,
			source: ValueSource::User {
				position: DatumPos {
					file: self.file_id,
					full_range: constdef.syntax().text_range(),
					name_range: const_name.text_range(),
				},
				ast: constdef.syntax().clone(),
			},
			kind: ValueKind::Constant,
		});

		self.project
			.globals_mut()
			.insert(iname, Rc::new(project::Datum::ZScript(datum)));

		self.contributed.push(iname);
	}

	fn register_enum_variants(
		&self,
		datum: &mut EnumDatum,
		enumdef: ast::EnumDef,
		mut parent_scope: Option<&mut Scope>,
	) {
		for variant in enumdef.variants() {
			let iname = self.strings.value_name_nocase(variant.name().text());
			datum.variants.insert(iname);

			if let Some(scope) = parent_scope.as_mut() {
				scope.insert(
					iname,
					Rc::new(project::Datum::ZScript(Datum::Value(ValueDatum {
						name: iname,
						source: ValueSource::User {
							position: DatumPos {
								file: self.file_id,
								full_range: variant.syntax().text_range(),
								name_range: variant.name().text_range(),
							},
							ast: variant.syntax().clone(),
						},
						kind: ValueKind::EnumVariant,
					}))),
				);
			}
		}
	}

	fn register_function_decl(&self, fndecl: ast::FunctionDecl, scope: &mut Scope) {
		if fndecl.param_list().is_err() {
			return;
		}

		let Some(body_ast) = fndecl.body() else { return; };

		let iname = self.strings.value_name_nocase(fndecl.name().text());

		let mut dat_fn = FunctionDatum {
			name: iname,
			source: FunctionSource::User {
				position: DatumPos {
					file: self.file_id,
					full_range: fndecl.syntax().text_range(),
					name_range: fndecl.name().text_range(),
				},
				ast: fndecl.clone(),
			},
			is_const: fndecl.is_const(),
			is_static: fndecl
				.qualifiers()
				.iter()
				.any(|qual| matches!(qual, ast::MemberQual::Static(_))),
			body: None,
		};

		let mut body = FunctionBody {
			cflow: DiGraph::default(),
			block_map: FxHashMap::default(),
		};

		let ix_entry = body.cflow.add_node(Rc::new(Scope::default()));
		body.block_map
			.insert(body_ast.syntax().text_range(), ix_entry);

		for _ in body_ast.innards() {
			// TODO: ???
		}

		dat_fn.body = Some(body);
		scope.insert(
			iname,
			Rc::new(project::Datum::ZScript(Datum::Function(dat_fn))),
		);
	}
}

// Name resolution /////////////////////////////////////////////////////////////

#[derive(Debug)]
pub(crate) struct NameResContext<'c> {
	pub(crate) project_ix: usize,
	pub(crate) strings: &'c StringInterner,
	pub(crate) scopes: ScopeStack,
	pub(crate) parsed: &'c mut ParsedFile,
}

impl NameResContext<'_> {
	pub(crate) fn resolve_names(&mut self) {
		self.parsed.resolved.clear();
		let cursor = SyntaxNode::new_root(self.parsed.green.clone());

		for child in cursor.children() {
			let Some(top) = ast::TopLevel::cast(child) else { continue; };

			match top {
				ast::TopLevel::ClassDef(classdef) => {
					self.resolve_names_classdef(classdef);
				}
				ast::TopLevel::ClassExtend(_) => todo!(),
				ast::TopLevel::ConstDef(_) => todo!(),
				ast::TopLevel::EnumDef(enumdef) => {
					self.resolve_names_enumdef(enumdef);
				}
				ast::TopLevel::MixinClassDef(_) => todo!(),
				ast::TopLevel::StructDef(_) => todo!(),
				ast::TopLevel::StructExtend(_) => todo!(),
				ast::TopLevel::Include(_) | ast::TopLevel::Version(_) => continue,
			}
		}
	}

	fn resolve_names_classdef(&mut self, classdef: ast::ClassDef) {
		let iname = self
			.strings
			.type_name_nocase(classdef.name().unwrap().text());
		let datum = self.scopes.lookup(iname).unwrap().clone();

		let project::Datum::ZScript(Datum::Class(dat_class)) = datum.as_ref() else {
			unreachable!()
		};

		// Parent and `replaces` clauses ///////////////////////////////////////

		if let Some(anc_iname) = dat_class.parent {
			if let Some(d) = self.scopes.lookup(anc_iname).cloned() {
				self.parsed
					.resolved
					.insert(classdef.parent_class().unwrap().text_range(), d);
			}
		}

		for qual in classdef.qualifiers() {
			let ast::ClassQual::Replaces(replaces) = qual else {
				continue;
			};

			let Ok(ident) = replaces.replaced() else {
				continue;
			};

			let r_iname = self.strings.get_or_intern_nocase(ident.text());

			let Some(d) = self.scopes.lookup(IName::Type(r_iname)) else {
				continue;
			};

			self.parsed.resolved.insert(ident.text_range(), d.clone());
		}

		// Push class' scope and its ancestors' scopes /////////////////////////

		let len = self.scopes.len();
		let mut rel_datum = datum.clone();

		loop {
			let project::Datum::ZScript(Datum::Class(d)) = rel_datum.as_ref() else {
				unreachable!()
			};

			let Some(anc_iname) = d.parent else {
				// `d` is `Object`; it inherits from nothing.
				break;
			};

			let Some(anc_datum) = self.scopes.lookup(anc_iname).cloned() else {
				break;
			};

			let project::Datum::ZScript(Datum::Class(p)) = anc_datum.as_ref() else {
				unreachable!()
			};

			self.scopes.insert(
				len,
				StackedScope {
					project_ix: Some(self.project_ix),
					inner: p.scope.clone(),
					is_addendum: true,
				},
			);

			rel_datum = anc_datum;
		}

		self.scopes.push(StackedScope {
			project_ix: Some(self.project_ix),
			inner: dat_class.scope.clone(),
			is_addendum: false,
		});

		// Body ////////////////////////////////////////////////////////////////

		for innard in classdef.innards() {
			match innard {
				ast::ClassInnard::Field(field) => {
					if let Ok(type_spec) = field.type_spec() {
						self.resolve_names_typeref(type_spec);
					}
				}
				ast::ClassInnard::Enum(enumdef) => {
					self.resolve_names_enumdef(enumdef);
				}
				// TODO
				_ => {}
			}
		}

		self.scopes.pop_with_addenda();
	}

	fn resolve_names_enumdef(&mut self, _: ast::EnumDef) {
		// TODO
	}

	fn resolve_names_expr(&mut self, _: ast::Expr) {
		// TODO
	}

	fn resolve_names_typeref(&mut self, typeref: ast::TypeRef) {
		match typeref.core() {
			ast::CoreType::DynArray(arr_t) => {
				if let Ok(tref) = arr_t.element_type() {
					self.resolve_names_typeref(tref);
				}
			}
			// TODO
			ast::CoreType::Let(_) => {
				// Valid parse, invalid semantically.
				return;
			}
			_ => {}
		}

		if let Some(array_len) = typeref.array_len() {
			if let Some(expr) = array_len.expr() {
				self.resolve_names_expr(expr);
			}
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
