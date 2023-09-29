use doomfront::{
	rowan::{ast::AstNode, Language},
	zdoom::{
		self,
		zscript::{ast, Syn},
	},
};
use lsp_types::{DiagnosticRelatedInformation, DiagnosticSeverity};
use rustc_hash::FxHashMap;

use crate::{
	core::{DefIx, Definition, SymGraphKey, SymGraphValue, SymIx},
	frontend::FrontendContext,
	langs::zscript::sema::{self, Datum, FunctionFlags},
};

#[must_use]
pub(crate) fn define(ctx: &FrontendContext, sym_ix: SymIx, ast: ast::FunctionDecl) -> DefIx {
	let holder = ctx.holder_of(sym_ix);
	let holder_syn = Syn::kind_from_raw(holder.syn);

	ctx.make_ref_to(ast.name().text_range(), sym_ix);

	if holder_syn == Syn::MixinClassDef {
		// TODO: What can reasonably be done about these?
		return DefIx::PLACEHOLDER;
	}

	let is_class_member = match holder_syn {
		Syn::ClassDef => true,
		Syn::StructDef => false,
		_ => unreachable!(),
	};

	let mut datum = sema::Function {
		flags: FunctionFlags::empty(),
		scope: sema::Scope::Data,
		_min_version: zdoom::Version::V1_0_0,
		vis: sema::Visibility::Public,
		deprecated: None,
	};

	if is_class_member {
		class_function(ctx, sym_ix, ast, &mut datum);
	} else {
		struct_function(ctx, sym_ix, ast, &mut datum);
	}

	let def_ix = ctx.defs.push(Definition::ZScript(Datum::Function(datum)));
	DefIx(def_ix as u32)
}

fn class_function(
	ctx: &FrontendContext,
	sym_ix: SymIx,
	ast: ast::FunctionDecl,
	datum: &mut sema::Function,
) {
	let quals = ast.qualifiers();
	let qualset = set_of_qualifiers(ctx, &quals);

	process_scope_qualifiers(ctx, &quals, &qualset, datum);

	if let Some(sgn) = ctx.sym_graph.get(&SymGraphKey::PrototypeFor(sym_ix)) {
		let SymGraphValue::Symbol(proto_ix) = sgn.value() else {
			unreachable!();
		};

		let proto_def = ctx.get_or_require_def(*proto_ix, |ctx, sym_ix, sym| {
			let node = ctx.src.node_covering(sym.id.span);
			let fndecl = ast::FunctionDecl::cast(node).unwrap();
			define(ctx, sym_ix, fndecl)
		});

		let Definition::ZScript(Datum::Function { .. }) = proto_def else {
			unreachable!()
		};
	} else {
		// This function isn't overriding anything.
		// Does it have invalid qualifiers?
		// TODO: handle `static` qualifier.
	}
}

fn struct_function(
	ctx: &FrontendContext,
	_: SymIx,
	ast: ast::FunctionDecl,
	datum: &mut sema::Function,
) {
	let quals = ast.qualifiers();
	let qualset = set_of_qualifiers(ctx, &quals);

	process_scope_qualifiers(ctx, &quals, &qualset, datum);

	match (qualset.get(&Syn::KwStatic), ast.const_keyword()) {
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

	match (qualset.get(&Syn::KwProtected), qualset.get(&Syn::KwPrivate)) {
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

	if let Some(q) = qualset.get(&Syn::ActionQual) {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"only methods of classes inheriting from `Actor` can be marked as actions".to_string(),
		));
	}

	if let Some(q) = qualset.get(&Syn::KwFinal) {
		// Note that as of GZDoom 4.10.0, this is accepted silently by the compiler.
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::WARNING,
			"`final` qualifier does nothing for struct methods".to_string(),
		));
	}

	if let Some(q) = qualset.get(&Syn::KwInternal) {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::WARNING,
			"`internal` has no applications for user code".to_string(),
		));
	}

	if let Some(q) = qualset.get(&Syn::KwNative) {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"the `native` keyword is forbidden to user code".to_string(),
		));
	}

	if let Some(q) = qualset.get(&Syn::KwReadOnly) {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"`readOnly` is only applicable to member variables, not functions".to_string(),
		));
	}

	if let Some(q) = qualset.get(&Syn::KwTransient) {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"`transient` is only applicable to member variables, not functions".to_string(),
		));
	}

	if let Some(q) = qualset.get(&Syn::KwVarArg) {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"`varArg` is only usable by native functions".to_string(),
		));
	}

	// Note that, as of GZDoom 4.10.0, none of these are a compiler error,
	// but using any will cause a segfault.

	if let Some(q) = qualset.get(&Syn::KwAbstract) {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"struct methods cannot be marked as `abstract`".to_string(),
		));
	}

	if let Some(q) = qualset.get(&Syn::KwOverride) {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"struct methods cannot be marked as `override`".to_string(),
		));
	}

	if let Some(q) = qualset.get(&Syn::KwVirtual) {
		ctx.raise(ctx.src.diag_builder(
			q.text_range(),
			DiagnosticSeverity::ERROR,
			"struct methods cannot be marked as `virtual`".to_string(),
		));
	}
}

#[must_use]
fn set_of_qualifiers(
	ctx: &FrontendContext,
	quals: &ast::MemberQuals,
) -> FxHashMap<Syn, ast::MemberQual> {
	let mut qualset = FxHashMap::default();

	for qual in quals.iter() {
		let displaced = qualset.insert(qual.kind(), qual);

		if let Some(d_q) = displaced {
			// Note that as of GZDoom 4.10.0, this is accepted silently by the compiler,
			// even if it's a repeated `version` qualifier.
			ctx.raise(ctx.src.diag_builder(
				d_q.text_range(),
				DiagnosticSeverity::WARNING,
				"repeating a qualifier does nothing".to_string(),
			));
		}
	}

	qualset
}

fn process_scope_qualifiers(
	ctx: &FrontendContext,
	quals: &ast::MemberQuals,
	qualset: &FxHashMap<Syn, ast::MemberQual>,
	datum: &mut sema::Function,
) {
	match (
		qualset.get(&Syn::KwClearScope),
		qualset.get(&Syn::KwPlay),
		qualset.get(&Syn::KwUi),
		qualset.get(&Syn::KwVirtualScope),
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
}
