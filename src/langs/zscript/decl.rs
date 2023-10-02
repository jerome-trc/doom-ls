//! The ZScript frontend's first phase: symbol declaration.

use doomfront::{
	rowan::{ast::AstNode, TextRange},
	zdoom::zscript::ast,
};
use lsp_types::{DiagnosticRelatedInformation, DiagnosticSeverity};

use crate::{
	core::{Scope, SymIx},
	frontend::FrontendContext,
	intern::NsName,
	langs::LangId,
};

pub(crate) fn declare_class(ctx: &FrontendContext, ast: ast::ClassDef) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let crit_end = if let Some(qual) = ast.qualifiers().last() {
		qual.text_range().end()
	} else if let Some(parent) = ast.parent_class() {
		parent.text_range().end()
	} else {
		ast.name().unwrap().text_range().end()
	};

	let crit_span = TextRange::new(ast.keyword().text_range().start(), crit_end);

	let mut globals = ctx.global_scope_mut(ctx.project_ix);

	let result = ctx.declare(
		&mut globals,
		ns_name,
		LangId::ZScript,
		ast.syntax(),
		crit_span,
	);

	drop(globals);

	if let Err(prev) = result {
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

	let crit_span = TextRange::new(
		ast.keyword().text_range().start(),
		ast.syntax().text_range().end(),
	);

	let result = if let Some(o) = outer {
		ctx.declare(o, ns_name, LangId::ZScript, ast.syntax(), crit_span)
	} else {
		let mut globals = ctx.global_scope_mut(ctx.project_ix);

		ctx.declare(
			&mut globals,
			ns_name,
			LangId::ZScript,
			ast.syntax(),
			crit_span,
		)
	};

	if let Err(prev) = result {
		redeclare_error(ctx, prev, crit_span, ident.text());
	}
}

pub(crate) fn declare_enum(
	ctx: &FrontendContext,
	mut outer: Option<&mut Scope>,
	ast: ast::EnumDef,
) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let crit_end = if let Some(tspec) = ast.type_spec() {
		tspec.0.text_range().end()
	} else {
		ast.keyword().text_range().end()
	};

	let crit_span = TextRange::new(ast.keyword().text_range().start(), crit_end);

	let result = if let Some(o) = outer.as_mut() {
		ctx.declare(o, ns_name, LangId::ZScript, ast.syntax(), crit_span)
	} else {
		let mut globals = ctx.global_scope_mut(ctx.project_ix);

		let r = ctx.declare(
			&mut globals,
			ns_name,
			LangId::ZScript,
			ast.syntax(),
			crit_span,
		);

		r
	};

	let enum_ix = match result {
		Ok(ix) => Some(ix),
		Err(prev) => {
			redeclare_error(ctx, prev, crit_span, ident.text());
			None
		}
	};

	for variant in ast.variants() {
		let v_ident = variant.name().into();
		let v_ns_name = NsName::Value(ctx.names.intern(&v_ident));

		let result = if let Some(o) = outer.as_mut() {
			ctx.declare(
				o,
				v_ns_name,
				LangId::ZScript,
				variant.syntax(),
				variant.syntax().text_range(),
			)
		} else {
			let mut globals = ctx.global_scope_mut(ctx.project_ix);

			ctx.declare(
				&mut globals,
				v_ns_name,
				LangId::ZScript,
				variant.syntax(),
				variant.syntax().text_range(),
			)
		};

		match result {
			Ok(ix) => if let Some(enum_ix) = enum_ix {
				ctx.make_member(ix, enum_ix);
			}
			Err(prev) => {
				redeclare_error(ctx, prev, variant.syntax().text_range(), v_ident.text());
			}
		}
	}
}

pub(crate) fn declare_mixin_class(ctx: &FrontendContext, ast: ast::MixinClassDef) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let crit_span = TextRange::new(
		ast.keywords().0.text_range().start(),
		ident.text_range().start(),
	);

	let mut globals = ctx.global_scope_mut(ctx.project_ix);

	let result = ctx.declare(
		&mut globals,
		ns_name,
		LangId::ZScript,
		ast.syntax(),
		crit_span,
	);

	drop(globals);

	if let Err(prev) = result {
		redeclare_error(ctx, prev, crit_span, ident.text());
	}
}

pub(crate) fn declare_static_const(
	ctx: &FrontendContext,
	outer: &mut Scope,
	ast: ast::StaticConstStat,
) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Value(ctx.names.intern(&ident));

	let crit_span = TextRange::new(
		ast.keywords().0.text_range().start(),
		ast.name().unwrap().text_range().end(),
	);

	let result = ctx.declare(outer, ns_name, LangId::ZScript, ast.syntax(), crit_span);

	if let Err(prev) = result {
		redeclare_error(ctx, prev, crit_span, ident.text());
	}
}

pub(crate) fn declare_struct(
	ctx: &FrontendContext,
	outer: Option<&mut Scope>,
	ast: ast::StructDef,
) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let crit_end = if let Some(qual) = ast.qualifiers().last() {
		qual.text_range().end()
	} else {
		ast.name().unwrap().text_range().end()
	};

	let crit_span = TextRange::new(ast.keyword().text_range().start(), crit_end);

	let result = if let Some(o) = outer {
		ctx.declare(o, ns_name, LangId::ZScript, ast.syntax(), crit_span)
	} else {
		let mut globals = ctx.global_scope_mut(ctx.project_ix);

		let r = ctx.declare(
			&mut globals,
			ns_name,
			LangId::ZScript,
			ast.syntax(),
			crit_span,
		);

		r
	};

	if let Err(prev) = result {
		redeclare_error(ctx, prev, crit_span, ident.text());
	}
}

// Details /////////////////////////////////////////////////////////////////////

pub(super) fn redeclare_error(
	ctx: &FrontendContext,
	prev: SymIx,
	crit_span: TextRange,
	name_str: &str,
) {
	let mut b = ctx.src.diag_builder(
		crit_span,
		DiagnosticSeverity::ERROR,
		format!("attempt to re-declare symbol `{}`", name_str),
	);

	if let Some(location) = ctx.location_of(prev) {
		b = b.with_related(DiagnosticRelatedInformation {
			location,
			message: "original declaration is here".to_string(),
		});
	}

	ctx.raise(b);
}
