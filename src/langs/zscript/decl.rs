//! The ZScript frontend's first phase: symbol declaration.

use doomfront::{
	rowan::{ast::AstNode, TextRange},
	zdoom::zscript::ast,
};
use lsp_types::{DiagnosticRelatedInformation, DiagnosticSeverity};

use crate::{
	arena::Arena, core::Scope, data::SymPtr, frontend::FrontendContext, intern::NsName,
	langs::LangId,
};

use super::expand;

pub(crate) fn declare_class(ctx: &FrontendContext, ast: ast::ClassDef) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let mut globals = ctx.global_scope_mut(ctx.project_ix);

	let result = ctx.declare(&mut globals, ns_name, LangId::ZScript, ast.syntax());

	drop(globals);

	if let Err(prev) = result {
		let crit_span = super::symbol_crit_span(ast.syntax());
		redeclare_error(ctx, prev, crit_span, ident.text());
	}
}

pub(crate) fn declare_constant(
	ctx: &FrontendContext,
	outer: Option<&mut Scope>,
	ast: ast::ConstDef,
) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Value(ctx.names.intern(&ident));

	let result = if let Some(o) = outer {
		ctx.declare(o, ns_name, LangId::ZScript, ast.syntax())
	} else {
		let mut globals = ctx.global_scope_mut(ctx.project_ix);
		ctx.declare(&mut globals, ns_name, LangId::ZScript, ast.syntax())
	};

	if let Err(prev) = result {
		redeclare_error(
			ctx,
			prev,
			super::symbol_crit_span(ast.syntax()),
			ident.text(),
		);
	}
}

pub(crate) fn declare_enum(
	ctx: &FrontendContext,
	mut outer: Option<&mut Scope>,
	ast: ast::EnumDef,
) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let result = if let Some(o) = outer.as_mut() {
		ctx.declare(o, ns_name, LangId::ZScript, ast.syntax())
	} else {
		let mut globals = ctx.global_scope_mut(ctx.project_ix);
		ctx.declare(&mut globals, ns_name, LangId::ZScript, ast.syntax())
	};

	let enum_ptr = match result {
		Ok(ix) => Some(ix),
		Err(prev) => {
			redeclare_error(
				ctx,
				prev,
				super::symbol_crit_span(ast.syntax()),
				ident.text(),
			);
			None
		}
	};

	for variant in ast.variants() {
		let v_ident = variant.name().into();
		let v_ns_name = NsName::Value(ctx.names.intern(&v_ident));

		let result = if let Some(o) = outer.as_mut() {
			ctx.declare(o, v_ns_name, LangId::ZScript, variant.syntax())
		} else {
			let mut globals = ctx.global_scope_mut(ctx.project_ix);

			ctx.declare(&mut globals, v_ns_name, LangId::ZScript, variant.syntax())
		};

		match result {
			Ok(variant_ptr) => {
				if let Some(e) = &enum_ptr {
					ctx.make_member(variant_ptr, e.clone());
				}
			}
			Err(prev) => {
				redeclare_error(ctx, prev, variant.syntax().text_range(), v_ident.text());
			}
		}
	}
}

pub(crate) fn declare_field(
	ctx: &FrontendContext,
	holder: SymPtr,
	outer: &mut Scope,
	ast: ast::FieldDecl,
) {
	for var_name in ast.names() {
		let ident = var_name.ident().into();
		let ns_name = NsName::Value(ctx.names.intern(&ident));

		let result = ctx.declare(outer, ns_name, LangId::ZScript, var_name.syntax());

		match result {
			Ok(declared) => {
				ctx.make_member(declared, holder.clone());
			}
			Err(prev) => {
				redeclare_error(
					ctx,
					prev,
					super::symbol_crit_span(var_name.syntax()),
					ident.text(),
				);
			}
		}
	}
}

pub(crate) fn declare_flagdef(
	ctx: &FrontendContext,
	holder: SymPtr,
	outer: &mut Scope,
	ast: ast::FlagDef,
) {
	let ident = ast.name().unwrap().into();

	let result = ctx.declare(
		outer,
		NsName::FlagDef(ctx.names.intern(&ident)),
		LangId::ZScript,
		ast.syntax(),
	);

	match result {
		Ok(ix) => {
			ctx.make_member(ix, holder.clone());
		}
		Err(prev) => {
			redeclare_error(
				ctx,
				prev,
				super::symbol_crit_span(ast.syntax()),
				ident.text(),
			);
		}
	};

	let mut varname = format!("b{}", ident.text());

	{
		let second_char = &mut varname[1..2];
		second_char.make_ascii_uppercase();
	}

	// Note that, as of GZDoom 4.10.0, shadowing is silently accepted by the compiler.

	let result = ctx.declare(
		outer,
		NsName::Value(ctx.names.intern_str(&varname)),
		LangId::ZScript,
		ast.syntax(),
	);

	match result {
		Ok(ix) => {
			ctx.make_member(ix, holder);
		}
		Err(prev) => {
			let mut b = ctx.src.diag_builder(
				super::symbol_crit_span(ast.syntax()),
				DiagnosticSeverity::ERROR,
				format!("flagdef's fake boolean `{varname}` shadows a field"),
			);

			if let Some(u_sym) = prev.as_user() {
				b = b.with_related(DiagnosticRelatedInformation {
					location: ctx.diag_location(u_sym, super::symbol_crit_span),
					message: "field is declared here".to_string(),
				});
			}

			ctx.raise(b);
		}
	}
}

pub(crate) fn declare_function(
	ctx: &FrontendContext,
	holder: SymPtr,
	outer: &mut Scope,
	ast: ast::FunctionDecl,
	class: bool,
) {
	let ident = ast.name().into();
	let ns_name = NsName::Value(ctx.names.intern(&ident));

	if !class {
		let result = ctx.declare(outer, ns_name, LangId::ZScript, ast.syntax());

		match result {
			Ok(ix) => {
				ctx.make_member(ix, holder);
			}
			Err(prev) => {
				redeclare_error(
					ctx,
					prev,
					super::symbol_crit_span(ast.syntax()),
					ident.text(),
				);
			}
		}
	} else {
		let (overrider, overriden_opt) =
			ctx.decl_override(outer, ns_name, LangId::ZScript, ast.syntax());

		ctx.make_member(overrider.clone(), holder);

		if let Some(overriden) = overriden_opt {
			ctx.make_override_of(overriden, overrider);
		}
	}
}

pub(crate) fn declare_mixin_class(ctx: &FrontendContext, ast: ast::MixinClassDef) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let result = {
		let mut globals = ctx.global_scope_mut(ctx.project_ix);

		ctx.declare_and(
			&mut globals,
			ns_name,
			LangId::ZScript,
			ast.syntax(),
			|mixin_ptr| {
				let mixin_u = mixin_ptr.as_user().unwrap();
				let mut scope = Scope::default();
				expand::declare_class_innards(ctx, mixin_ptr.clone(), &mut scope, ast.innards());
				let mut bump = ctx.arena.borrow();
				let scope_ptr = Arena::alloc(&mut bump, scope);
				mixin_u.scope.store(scope_ptr.as_ptr().unwrap());
			},
		)
	};

	if let Err(prev) = result {
		redeclare_error(
			ctx,
			prev,
			super::symbol_crit_span(ast.syntax()),
			ident.text(),
		);
	}
}

pub(crate) fn declare_property(
	ctx: &FrontendContext,
	holder: SymPtr,
	outer: &mut Scope,
	ast: ast::PropertyDef,
) {
	let ident = ast.name().unwrap().into();

	let result = ctx.declare(
		outer,
		NsName::Property(ctx.names.intern(&ident)),
		LangId::ZScript,
		ast.syntax(),
	);

	match result {
		Ok(ix) => {
			ctx.make_member(ix, holder);
		}
		Err(prev) => {
			redeclare_error(
				ctx,
				prev,
				super::symbol_crit_span(ast.syntax()),
				ident.text(),
			);
		}
	}
}

pub(crate) fn declare_state_labels(
	ctx: &FrontendContext,
	holder: SymPtr,
	outer: &mut Scope,
	ast: ast::StatesBlock,
) {
	for innard in ast.innards() {
		let ast::StatesInnard::Label(label) = innard else {
			continue;
		};

		let name_tok = label.name().into();
		let ns_name = NsName::StateLabel(ctx.names.intern(&name_tok));

		let result = ctx.declare(outer, ns_name, LangId::ZScript, label.syntax());

		match result {
			Ok(ix) => {
				ctx.make_member(ix, holder.clone());
			}
			Err(prev) => {
				redeclare_error(
					ctx,
					prev,
					super::symbol_crit_span(label.syntax()),
					name_tok.text(),
				);
			}
		}
	}
}

pub(crate) fn declare_static_const(
	ctx: &FrontendContext,
	outer: &mut Scope,
	ast: ast::StaticConstStat,
) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Value(ctx.names.intern(&ident));

	let result = ctx.declare(outer, ns_name, LangId::ZScript, ast.syntax());

	if let Err(prev) = result {
		redeclare_error(
			ctx,
			prev,
			super::symbol_crit_span(ast.syntax()),
			ident.text(),
		);
	}
}

pub(crate) fn declare_struct(
	ctx: &FrontendContext,
	outer: Option<&mut Scope>,
	ast: ast::StructDef,
) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let result = if let Some(o) = outer {
		ctx.declare_and(o, ns_name, LangId::ZScript, ast.syntax(), |struct_ptr| {
			let struct_u = struct_ptr.as_user().unwrap();
			let mut scope = Scope::default();
			expand::declare_struct_innards(ctx, struct_ptr.clone(), &mut scope, ast.innards());
			let mut bump = ctx.arena.borrow();
			let scope_ptr = Arena::alloc(&mut bump, scope);
			struct_u.scope.store(scope_ptr.as_ptr().unwrap());
		})
	} else {
		let mut globals = ctx.global_scope_mut(ctx.project_ix);

		ctx.declare_and(
			&mut globals,
			ns_name,
			LangId::ZScript,
			ast.syntax(),
			|struct_ptr| {
				let struct_u = struct_ptr.as_user().unwrap();
				let mut scope = Scope::default();
				expand::declare_struct_innards(ctx, struct_ptr.clone(), &mut scope, ast.innards());
				let mut bump = ctx.arena.borrow();
				let scope_ptr = Arena::alloc(&mut bump, scope);
				struct_u.scope.store(scope_ptr.as_ptr().unwrap());
			},
		)
	};

	if let Err(prev) = result {
		redeclare_error(
			ctx,
			prev,
			super::symbol_crit_span(ast.syntax()),
			ident.text(),
		);
	}
}

// Details /////////////////////////////////////////////////////////////////////

pub(super) fn redeclare_error(
	ctx: &FrontendContext,
	prev: SymPtr,
	crit_span: TextRange,
	name_str: &str,
) {
	let mut b = ctx.src.diag_builder(
		crit_span,
		DiagnosticSeverity::ERROR,
		format!("attempt to re-declare symbol `{}`", name_str),
	);

	if let Some(u_sym) = prev.as_user() {
		b = b.with_related(DiagnosticRelatedInformation {
			location: ctx.diag_location(u_sym, super::symbol_crit_span),
			message: "original declaration is here".to_string(),
		});
	}

	ctx.raise(b);
}
