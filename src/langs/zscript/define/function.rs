use doomfront::{
	rowan::ast::AstNode,
	zdoom::zscript::ast::{self, MemberQualSet},
};
use lsp_types::{DiagnosticRelatedInformation, DiagnosticSeverity};

use crate::{
	arena::Arena,
	data::{Definition, SymGraphKey, SymPtr},
	frontend::FrontendContext,
	langs::zscript::sema::{self, FunctionFlags},
};

pub(crate) fn define(
	ctx: &FrontendContext,
	sym_ptr: SymPtr,
	ast: ast::FunctionDecl,
	class_member: bool,
) {
	let fn_u = sym_ptr.as_user().unwrap();
	ctx.make_ref_to(ast.name().text_range(), sym_ptr.clone());

	let mut datum = sema::Function {
		params: vec![],
		flags: FunctionFlags::empty(),
		scope: sema::Scope::Data, // TODO: inherit from holder.
		min_version: None,
		vis: sema::Visibility::Public,
		deprecated: None,
	};

	let quals = ast.qualifiers();
	let qualset = MemberQualSet::new(&quals, |prev_span, repeated| {
		match repeated {
			ast::MemberQual::Action(_) => todo!(),
			ast::MemberQual::Deprecation(_) => todo!(),
			ast::MemberQual::Version(_) => todo!(),
			ast::MemberQual::Abstract(_) => todo!(),
			ast::MemberQual::ClearScope(_) => todo!(),
			ast::MemberQual::Final(_) => todo!(),
			ast::MemberQual::Override(_) => todo!(),
			ast::MemberQual::Static(_) => todo!(),
			ast::MemberQual::Virtual(_) => todo!(),
			ast::MemberQual::VirtualScope(_) => todo!(),
			// Scope qualifiers get processed later.
			ast::MemberQual::Play(_) | ast::MemberQual::Ui(_) => {}
			// Visibility qualifiers get processed later.
			ast::MemberQual::Private(_) | ast::MemberQual::Protected(_) => {}
			// Useless to user code. Diagnostics get raised about these later.
			ast::MemberQual::Internal(_)
			| ast::MemberQual::Native(_)
			| ast::MemberQual::VarArg(_) => {}
			// Inapplicable to functions. Diagnostics get raised about these later.
			ast::MemberQual::Meta(_)
			| ast::MemberQual::ReadOnly(_)
			| ast::MemberQual::Transient(_) => {}
		}
	});

	if class_member {
		class_function(ctx, sym_ptr.clone(), ast, quals, qualset, &mut datum);
	} else {
		struct_function(ctx, sym_ptr.clone(), ast, quals, qualset, &mut datum);
	}

	let mut bump = ctx.arena.borrow();
	let def_ptr = Arena::alloc(&mut bump, Definition::ZScript(sema::Datum::Function(datum)));
	fn_u.def.store(def_ptr.as_ptr().unwrap());
}

fn class_function(
	ctx: &FrontendContext,
	sym_ptr: SymPtr,
	ast: ast::FunctionDecl,
	quals: ast::MemberQuals,
	qualset: MemberQualSet,
	datum: &mut sema::Function,
) {
	process_qualifiers(ctx, &ast, &quals, &qualset, datum);

	// TODO
	if let Some(sgn) = ctx.sym_graph.get(&SymGraphKey::PrototypeFor(sym_ptr)) {
		let _ = sgn.as_symbol().unwrap();
		drop(sgn);
	} else {
		// This function isn't overriding anything.
		// Does it have invalid qualifiers?
	}
}

fn struct_function(
	ctx: &FrontendContext,
	_: SymPtr,
	ast: ast::FunctionDecl,
	quals: ast::MemberQuals,
	qualset: MemberQualSet,
	datum: &mut sema::Function,
) {
	process_qualifiers(ctx, &ast, &quals, &qualset, datum);

	match (qualset.q_protected.as_ref(), qualset.q_private.as_ref()) {
		(None, None) => {}
		(None, Some(_private_kw)) => {
			datum.vis = sema::Visibility::Private;
		}
		(Some(protected_kw), None) => {
			datum.vis = sema::Visibility::Protected;

			ctx.raise(ctx.src.diag_builder(
				protected_kw.text_range(),
				DiagnosticSeverity::INFORMATION,
				"`protected` is the same as `private` for structs".to_string(),
			));
		}
		(Some(private_kw), Some(protected_kw)) => {
			datum.vis = sema::Visibility::Private;

			ctx.raise(
				ctx.src
					.diag_builder(
						private_kw.text_range(),
						DiagnosticSeverity::INFORMATION,
						"`private` always overrides `protected`".to_string(),
					)
					.with_related(DiagnosticRelatedInformation {
						location: ctx.make_location(ctx.src, protected_kw.text_range()),
						message: "`protected` is the same as `private` for structs".to_string(),
					}),
			);
		}
	}

	if let Some(q) = qualset.q_action.as_ref() {
		ctx.raise(ctx.src.diag_builder(
			q.syntax().text_range(),
			DiagnosticSeverity::ERROR,
			"only methods of classes inheriting from `Actor` can be marked as actions".to_string(),
		));
	}

	if let Some(q) = qualset.q_final.as_ref() {
		// Note that as of GZDoom 4.10.0, this is accepted silently by the compiler.
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::WARNING,
			"`final` qualifier does nothing for struct methods".to_string(),
		));
	}

	if let Some(q) = qualset.q_internal.as_ref() {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::WARNING,
			"`internal` has no applications for user code".to_string(),
		));
	}

	if let Some(q) = qualset.q_native.as_ref() {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"the `native` keyword is forbidden to user code".to_string(),
		));
	}

	if let Some(q) = qualset.q_readonly.as_ref() {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"`readOnly` is only applicable to member variables, not functions".to_string(),
		));
	}

	if let Some(q) = qualset.q_transient.as_ref() {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"`transient` is only applicable to member variables, not functions".to_string(),
		));
	}

	if let Some(q) = qualset.q_vararg.as_ref() {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"`varArg` is only usable by native functions".to_string(),
		));
	}

	// Note that, as of GZDoom 4.10.0, none of these are a compiler error,
	// but using any will cause a segfault.

	if let Some(q) = qualset.q_abstract.as_ref() {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"struct methods cannot be marked as `abstract`".to_string(),
		));
	}

	if let Some(q) = qualset.q_override.as_ref() {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"struct methods cannot be marked as `override`".to_string(),
		));
	}

	if let Some(q) = qualset.q_virtual.as_ref() {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"struct methods cannot be marked as `virtual`".to_string(),
		));
	}
}

fn process_qualifiers(
	ctx: &FrontendContext,
	ast: &ast::FunctionDecl,
	quals: &ast::MemberQuals,
	qualset: &MemberQualSet,
	datum: &mut sema::Function,
) {
	match (
		qualset.q_clearscope.as_ref(),
		qualset.q_play.as_ref(),
		qualset.q_ui.as_ref(),
		qualset.q_virtualscope.as_ref(),
	) {
		(None, None, None, None) => {}
		(Some(_clearscope_kw), None, None, None) => {
			datum.scope = sema::Scope::Data;
		}
		(None, Some(_play_kw), None, None) => {
			datum.scope = sema::Scope::Play;
		}
		(None, None, Some(_ui_kw), None) => {
			datum.scope = sema::Scope::Ui;
		}
		(None, None, None, Some(_virtscope_kw)) => {
			datum.scope = sema::Scope::Virtual;
		}
		_ => {
			ctx.raise(ctx.src.diag_builder(
				quals.syntax().text_range(),
				DiagnosticSeverity::ERROR,
				"different scope qualifiers cannot be combined".to_string(),
			));
		}
	}

	match (qualset.q_static.as_ref(), ast.const_keyword()) {
		(None, None) => {}
		(None, Some(_const_kw)) => {
			datum.flags.insert(FunctionFlags::CONST);
		}
		(Some(_static_kw), None) => {
			datum.flags.insert(FunctionFlags::STATIC);
		}
		(Some(_), Some(const_kw)) => {
			ctx.raise(ctx.src.diag_builder(
				const_kw.text_range(),
				DiagnosticSeverity::ERROR,
				"`static` functions cannot be marked `const`".to_string(),
			));
		}
	}
}
