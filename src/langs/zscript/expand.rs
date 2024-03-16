use doomfront::{
	rowan::{ast::AstNode, cursor::SyntaxToken, Language, TextRange},
	zdoom::zscript::{ast, Syntax},
};
use lsp_types::{DiagnosticRelatedInformation, DiagnosticSeverity};

use crate::{
	arena::Arena,
	core::Scope,
	data::{Definition, InternalSymbol, SymPtr, Symbol},
	frontend::FrontendContext,
	intern::NsName,
	langs::LangId,
};

use super::{decl, sema::Datum};

#[must_use]
pub(crate) fn class_inheritance(
	ctx: &FrontendContext,
	sym_ptr: SymPtr,
	ast: ast::ClassDef,
	hierarchy: Vec<SymPtr>,
) -> Scope {
	let mut base_scope = if let Some(parent_ident) = ast.head().parent_class() {
		resolve_ancestry(ctx, sym_ptr.clone(), &ast, parent_ident.into(), hierarchy)
	} else {
		let parent_ptr = ctx.internal.cache_zscript.class_object.clone();
		ctx.make_child_of(parent_ptr.clone(), parent_ptr.clone());
		parent_ptr
			.as_internal()
			.unwrap()
			.scope
			.as_ref()
			.unwrap()
			.clone()
	};

	declare_class_innards(ctx, sym_ptr, &mut base_scope, ast.innards());
	base_scope
}

#[must_use]
fn resolve_ancestry(
	ctx: &FrontendContext,
	child_ptr: SymPtr,
	ast: &ast::ClassDef,
	parent_ident: SyntaxToken,
	hierarchy: Vec<SymPtr>,
) -> Scope {
	let parent_ns_name = NsName::Type(ctx.names.intern(&parent_ident));

	let parent_ptr = {
		let globals = ctx.global_scope(ctx.project_ix);

		let Some(parent_ptr) = globals.get(&parent_ns_name) else {
			ctx.raise(ctx.src.diag_builder(
				parent_ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"class `{}` has unknown base class `{}`",
					ast.head().name().unwrap().text(),
					parent_ident.text(),
				),
			));

			return Scope::default();
		};

		parent_ptr.clone()
	};

	if hierarchy.contains(&parent_ptr) {
		ctx.raise(ctx.src.diag_builder(
			parent_ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"class `{}` has circular inheritance",
				ast.head().name().unwrap().text(),
			),
		)); // TODO: how much richer can this error messaging be?

		return Scope::default();
	}

	if parent_ptr.lang() != LangId::ZScript {
		debug_assert_eq!(parent_ptr.lang(), LangId::Decorate);
		// Only ZScript and DECORATE symbols can use `NsName::Type`,
		// so this is the only kind of error that's applicable here.
		ctx.raise(ctx.src.diag_builder(
			parent_ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"ZScript class `{}` cannot inherit from DECORATE class `{}`",
				ast.head().name().unwrap().text(),
				parent_ident.text(),
			),
		));

		return Scope::default();
	}

	let result = match parent_ptr.as_ref().unwrap() {
		Symbol::User(_) => {
			validate_user_parent(ctx, parent_ptr.clone(), ast, parent_ident, hierarchy)
		}
		Symbol::Internal(in_sym) => validate_internal_parent(ctx, in_sym, ast, parent_ident),
	};

	if result.is_ok() {
		ctx.make_child_of(parent_ptr, child_ptr);
	}

	match result {
		Ok(scope) | Err(scope) => scope,
	}
}

fn validate_internal_parent(
	ctx: &FrontendContext,
	parent_sym: &InternalSymbol,
	ast: &ast::ClassDef,
	parent_ident: SyntaxToken,
) -> Result<Scope, Scope> {
	let Definition::ZScript(def) = &parent_sym.def else {
		unreachable!()
	};

	if matches!(def, Datum::Class { .. }) {
		return Ok(parent_sym.scope.as_ref().unwrap().clone());
	}

	let diag_builder = ctx.src.diag_builder(
		parent_ident.text_range(),
		DiagnosticSeverity::ERROR,
		format!(
			"class `{}` has unknown parent class `{}`",
			ast.head().name().unwrap().text(),
			parent_ident.text()
		),
	);

	let related_loc = ctx.make_location(
		ctx.src,
		TextRange::new(
			parent_ident.text_range().start(),
			parent_ident.text_range().start(),
		),
	);

	let message = match def {
		Datum::Enum(_) => {
			format!("`{}` is an enum", parent_ident.text())
		}
		Datum::_MixinClass => {
			format!("`{}` is a mixin class", parent_ident.text())
		}
		Datum::Primitive(_) => {
			format!("`{}` is a primitive type", parent_ident.text())
		}
		Datum::Struct(_) => {
			format!("`{}` is a struct", parent_ident.text())
		}
		Datum::Class(_) => unreachable!(), // Already handled.
		// None of these can be retrieved using `NsName::Type`.
		Datum::Constant | Datum::Field(_) | Datum::Function(_) => unreachable!(),
	};

	ctx.raise(diag_builder.with_related(DiagnosticRelatedInformation {
		location: related_loc,
		message,
	}));

	Err(Scope::default())
}

fn validate_user_parent(
	ctx: &FrontendContext,
	parent_sym: SymPtr,
	ast: &ast::ClassDef,
	parent_ident: SyntaxToken,
	mut hierarchy: Vec<SymPtr>,
) -> Result<Scope, Scope> {
	let parent_u = parent_sym.as_user().unwrap();
	let parent_syn = Syntax::kind_from_raw(parent_u.syn);

	if parent_syn != Syntax::ClassDef {
		let diag_builder = ctx.src.diag_builder(
			parent_ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"class `{}` has unknown parent class `{}`",
				ast.head().name().unwrap().text(),
				parent_ident.text()
			),
		);

		let related_loc = ctx.make_location(ctx.file_with(parent_u), parent_u.id.span);

		let message = match parent_syn {
			Syntax::EnumDef => format!("`{}` is an enum", parent_ident.text()),
			Syntax::StructDef => format!("`{}` is a struct", parent_ident.text()),
			Syntax::MixinClassDef => format!("`{}` is a mixin class", parent_ident.text()),
			// Nothing else can be retrieved using `NsName::Type`.
			_ => unreachable!(),
		};

		ctx.raise(diag_builder.with_related(DiagnosticRelatedInformation {
			location: related_loc,
			message,
		}));

		return Err(Scope::default());
	}

	hierarchy.push(parent_sym.clone());

	let ret = if let Some(p) = parent_u.scope.as_ref() {
		p.clone()
	} else {
		// Any class defined in a previous project should have already had its
		// inheritance fully resolved.
		debug_assert_eq!((parent_u.project as usize), ctx.project_ix);
		let src = ctx.file_with(parent_u);
		let node = src.node_covering::<Syntax>(parent_u.id.span);
		let new_ctx = FrontendContext { src, ..*ctx };

		let p = class_inheritance(
			&new_ctx,
			parent_sym.clone(),
			ast::ClassDef::cast(node).unwrap(),
			hierarchy,
		);

		let mut bump = ctx.arena.borrow();
		let scope_ptr = Arena::alloc(&mut bump, p.clone());
		parent_u.scope.store(scope_ptr.as_ptr().unwrap());
		p
	};

	Ok(ret)
}

pub(crate) fn declare_class_innards(
	ctx: &FrontendContext,
	class_ptr: SymPtr,
	scope: &mut Scope,
	innards: impl Iterator<Item = ast::ClassInnard>,
) {
	for innard in innards {
		match innard {
			ast::ClassInnard::Function(fndecl) => {
				decl::declare_function(ctx, class_ptr.clone(), scope, fndecl, true);
			}
			ast::ClassInnard::Field(field) => {
				decl::declare_field(ctx, class_ptr.clone(), scope, field);
			}
			ast::ClassInnard::Flag(flagdef) => {
				decl::declare_flagdef(ctx, class_ptr.clone(), scope, flagdef);
			}
			ast::ClassInnard::Enum(enumdef) => {
				decl::declare_enum(ctx, Some(scope), enumdef);
			}
			ast::ClassInnard::Const(constdef) => {
				decl::declare_constant(ctx, Some(scope), constdef);
			}
			ast::ClassInnard::States(states) => {
				decl::declare_state_labels(ctx, class_ptr.clone(), scope, states);
			}
			ast::ClassInnard::StaticConst(sconst) => {
				decl::declare_static_const(ctx, scope, sconst);
			}
			ast::ClassInnard::Property(property) => {
				decl::declare_property(ctx, class_ptr.clone(), scope, property);
			}
			ast::ClassInnard::Struct(structdef) => {
				decl::declare_struct(ctx, Some(scope), structdef);
			}
			ast::ClassInnard::Mixin(mixin_stat) => {
				expand_mixin(ctx, class_ptr.clone(), scope, mixin_stat);
			}
			ast::ClassInnard::Default(_) => continue,
		}
	}
}

pub(crate) fn declare_struct_innards(
	ctx: &FrontendContext,
	struct_ptr: SymPtr,
	scope: &mut Scope,
	innards: impl Iterator<Item = ast::StructInnard>,
) {
	for innard in innards {
		match innard {
			ast::StructInnard::Const(constdef) => {
				decl::declare_constant(ctx, Some(scope), constdef);
			}
			ast::StructInnard::Enum(enumdef) => {
				decl::declare_enum(ctx, Some(scope), enumdef);
			}
			ast::StructInnard::StaticConst(sconst) => {
				decl::declare_static_const(ctx, scope, sconst);
			}
			ast::StructInnard::Function(fndecl) => {
				decl::declare_function(ctx, struct_ptr.clone(), scope, fndecl, false);
			}
			ast::StructInnard::Field(field) => {
				decl::declare_field(ctx, struct_ptr.clone(), scope, field);
			}
		}
	}
}

pub(crate) fn extend_class(ctx: &FrontendContext, ast: ast::ClassExtend) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let extended_ptr = {
		let globals = ctx.global_scope(ctx.project_ix);

		let Some(e) = globals.get(&ns_name) else {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!("class `{}` not found", ident.text()),
			));

			return;
		};

		e.clone()
	};

	let Some(u_sym) = extended_ptr.as_user() else {
		ctx.raise(ctx.src.diag_builder(
			ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"internal symbol `{}` cannot be extended by user code",
				ident.text()
			),
		));

		return;
	};

	if u_sym.lang != LangId::ZScript {
		// Only ZScript and DECORATE symbols can use `NsName::Type`,
		// so this is the only kind of error that's applicable here.
		ctx.raise(ctx.src.diag_builder(
			ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"DECORATE class `{}` cannot be extended from ZScript",
				ident.text()
			),
		));

		return;
	}

	match Syntax::kind_from_raw(u_sym.syn) {
		Syntax::ClassDef => {}
		Syntax::EnumDef => {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!("`extend class` not applicable to enum `{}`", ident.text()),
			));

			return;
		}
		Syntax::StructDef => {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!("`extend class` not applicable to struct `{}`", ident.text()),
			));

			return;
		}
		Syntax::MixinClassDef => {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"`extend class` not applicable to mixin class `{}`",
					ident.text()
				),
			));

			return;
		}
		other => unreachable!("expected a symbol in the type namespace, found: {other:#?}"),
	}

	let mut scope = Scope::default();
	declare_class_innards(ctx, extended_ptr.clone(), &mut scope, ast.innards());

	let mut refmut = ctx.symbols.get_mut(&u_sym.id).unwrap();

	// SAFETY: the mutable DashMap reference is acting as a write lock on the `UserSymbol`.
	unsafe {
		let m = refmut.as_mut().unwrap();

		let Symbol::User(u_mut) = m else {
			unreachable!()
		};

		let scope_mut = u_mut.scope.as_mut().unwrap();

		for (ns_name, sym_p) in scope.into_iter() {
			let displaced = scope_mut.insert(ns_name, sym_p.clone());

			if let Some(d) = displaced {
				let node = ctx.src.node_covering(sym_p.as_user().unwrap().id.span);

				decl::redeclare_error(
					ctx,
					d,
					super::help::symbol_crit_span(&node),
					ctx.names.resolve(ns_name),
				);
			}
		}
	}
}

pub(crate) fn extend_struct(ctx: &FrontendContext, ast: ast::StructExtend) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let extended_ptr = {
		let globals = ctx.global_scope(ctx.project_ix);

		let Some(e) = globals.get(&ns_name) else {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!("struct `{}` not found", ident.text()),
			));

			return;
		};

		e.clone()
	};

	let Some(u_sym) = extended_ptr.as_user() else {
		ctx.raise(ctx.src.diag_builder(
			ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"internal symbol `{}` cannot be extended by user code",
				ident.text()
			),
		));

		return;
	};

	if u_sym.lang != LangId::ZScript {
		// Only ZScript and DECORATE symbols can use `NsName::Type`,
		// so this is the only kind of error that's applicable here.
		ctx.raise(ctx.src.diag_builder(
			ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"DECORATE class `{}` cannot be extended from ZScript",
				ident.text()
			),
		));

		return;
	}

	match Syntax::kind_from_raw(u_sym.syn) {
		Syntax::StructDef => {}
		Syntax::ClassDef => {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!("`extend struct` not applicable to class `{}`", ident.text()),
			));

			return;
		}
		Syntax::EnumDef => {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!("`extend struct` not applicable to enum `{}`", ident.text()),
			));

			return;
		}
		Syntax::MixinClassDef => {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"`extend struct` not applicable to mixin class `{}`",
					ident.text()
				),
			));

			return;
		}
		other => unreachable!("expected a symbol in the type namespace, found: {other:#?}"),
	}

	let mut scope = Scope::default();
	declare_struct_innards(ctx, extended_ptr.clone(), &mut scope, ast.innards());

	let mut refmut = ctx.symbols.get_mut(&u_sym.id).unwrap();

	// SAFETY: the mutable DashMap reference is acting as a write lock on the `UserSymbol`.
	unsafe {
		let m = refmut.as_mut().unwrap();

		let Symbol::User(u_mut) = m else {
			unreachable!()
		};

		let scope_mut = u_mut.scope.as_mut().unwrap();

		for (ns_name, sym_p) in scope.into_iter() {
			let displaced = scope_mut.insert(ns_name, sym_p.clone());

			if let Some(d) = displaced {
				let node = ctx.src.node_covering(sym_p.as_user().unwrap().id.span);

				decl::redeclare_error(
					ctx,
					d,
					super::help::symbol_crit_span(&node),
					ctx.names.resolve(ns_name),
				);
			}
		}
	}
}

fn expand_mixin(ctx: &FrontendContext, class_ptr: SymPtr, scope: &mut Scope, ast: ast::MixinStat) {
	let class_u = class_ptr.as_user().unwrap();
	let mixin_ident = ast.name().unwrap().into();
	let mixin_nsname = NsName::Type(ctx.names.intern(&mixin_ident));

	let mixin_ptr = {
		let globals = ctx.global_scope(ctx.project_ix);

		let Some(mixin_ptr) = globals.get(&mixin_nsname) else {
			ctx.raise(ctx.src.diag_builder(
				mixin_ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"class `{}` applies unknown mixin class `{}`",
					ctx.names.resolve(class_u.name),
					mixin_ident.text()
				),
			));

			return;
		};

		mixin_ptr.clone()
	};

	let mixin_u = mixin_ptr.as_user().unwrap();

	if let Err(()) = ctx.make_mixin(class_ptr.clone(), mixin_ptr.clone()) {
		ctx.raise(ctx.src.diag_builder(
			mixin_ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"mixin class `{}` has already been applied to class `{}`",
				mixin_ident.text(),
				ctx.names.resolve(class_u.name)
			),
		));

		return;
	}

	let new_ctx = FrontendContext {
		project_ix: mixin_u.project as usize,
		src: ctx.file_with(mixin_u),
		..*ctx
	};

	let node = new_ctx.src.node_covering::<Syntax>(mixin_u.id.span);
	let mixindef = ast::MixinClassDef::cast(node).unwrap();
	declare_class_innards(&new_ctx, class_ptr, scope, mixindef.innards());
}
