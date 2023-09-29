//! The ZScript frontend's first phase: symbol expansions.
//!
//! Recursively fill scopes with innards (e.g. declaring methods in classes),
//! resolving inheritance and performing mixin substitution and class/struct
//! extensions along the way.
//!
//! The symbol graph is populated with:
//! - holder-to-member relationships
//! - mixin-to-class relationships
//! - inheritance relationships
//! but most name-to-symbol reference relationships are deferred until phase 3.
//! The exception is mixin statements and extensions, which aren't revisited.

use doomfront::{
	rowan::{ast::AstNode, Language, NodeOrToken, TextRange},
	zdoom::zscript::{ast, Syn, SyntaxNode},
};
use lsp_types::{DiagnosticSeverity, OneOf};

use crate::{
	core::{Definition, Scope, SymIx, Symbol},
	frontend::FrontendContext,
	intern::NsName,
	langs::LangId,
};

use super::{decl, sema::Datum};

#[must_use]
pub(crate) fn declare_class_scope(
	ctx: &FrontendContext,
	sym_ix: SymIx,
	ast: ast::ClassDef,
) -> Option<Scope> {
	let mut scope = if let Some(parent_ident) = ast.parent_class() {
		let parent_ident = parent_ident.into();
		let parent_ns_name = NsName::Type(ctx.names.intern(&parent_ident));
		let globals = ctx.global_scope(ctx.project_ix);

		let Some(&parent_ix) = globals.get(&parent_ns_name) else {
			ctx.raise(ctx.src.diag_builder(
				parent_ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"class `{}` has unknown base class `{}`",
					ast.name().unwrap().text(),
					parent_ident.text(),
				),
			));

			return Some(Scope::default());
		};

		drop(globals);

		let sym = match ctx.symbol(parent_ix) {
			OneOf::Left(u_sym) => u_sym,
			OneOf::Right(in_sym) => {
				if in_sym.lang != LangId::ZScript {
					// TODO: raise an issue.
					return Some(Scope::default());
				}

				let Definition::ZScript(zs_def) = &in_sym.def else {
					unreachable!()
				};

				match zs_def {
					Datum::Class => return Some(in_sym.scope.clone()),
					// TODO: Forbid inheriting from primitives, structs, mixins, enums, etc.
					_ => unreachable!(),
				}
			}
		};

		if sym.lang != LangId::ZScript {
			// Only ZScript and DECORATE symbols can use `NsName::Type`,
			// so this is the only kind of error that's applicable here.
			ctx.raise(ctx.src.diag_builder(
				parent_ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"ZScript class `{}` cannot inherit from DECORATE class `{}`",
					ast.name().unwrap().text(),
					parent_ident.text(),
				),
			));

			return Some(Scope::default());
		}

		let syn = Syn::kind_from_raw(sym.syn);

		if syn != Syn::ClassDef {
			let message = match syn {
				Syn::StructDef => {
					format!(
						"ZScript class `{}` cannot inherit from struct `{}`",
						ast.name().unwrap().text(),
						parent_ident.text(),
					)
				}
				Syn::MixinClassDef => {
					format!(
						"ZScript class `{}` cannot inherit from mixin class `{}`",
						ast.name().unwrap().text(),
						parent_ident.text(),
					)
				}
				Syn::EnumDef => {
					format!(
						"ZScript class `{}` cannot inherit from enum `{}`",
						ast.name().unwrap().text(),
						parent_ident.text(),
					)
				}
				// Nothing else can use `NsName::Type`.
				_ => unreachable!(),
			};

			let src = ctx.file_with(sym);

			ctx.raise(src.diag_builder(
				parent_ident.text_range(),
				DiagnosticSeverity::ERROR,
				message,
			));

			return Some(Scope::default());
		}

		let Some(parent_scope) = require_scope(ctx, parent_ix, sym) else {
			return Some(Scope::default());
		};

		ctx.make_child_of(parent_ix, sym_ix);

		parent_scope
	} else {
		let parent_ix = SymIx::internal(ctx.internal.ixs_zscript.class_object);

		ctx.make_child_of(parent_ix, sym_ix);

		ctx.internal
			.by_index(ctx.internal.ixs_zscript.class_object)
			.scope
			.clone()
	};

	declare_class_innards(ctx, sym_ix, &mut scope, ast.innards());

	Some(scope)
}

pub(crate) fn declare_class_innards(
	ctx: &FrontendContext,
	sym_ix: SymIx,
	scope: &mut Scope,
	innards: impl Iterator<Item = ast::ClassInnard>,
) {
	for innard in innards {
		match innard {
			ast::ClassInnard::Function(fndecl) => {
				declare_function(ctx, sym_ix, scope, fndecl, true);
			}
			ast::ClassInnard::Field(field) => {
				declare_field(ctx, sym_ix, scope, field);
			}
			ast::ClassInnard::Flag(flagdef) => {
				declare_flagdef(ctx, sym_ix, scope, flagdef);
			}
			ast::ClassInnard::Enum(enumdef) => {
				decl::declare_enum(ctx, Some(scope), enumdef);
			}
			ast::ClassInnard::Const(constdef) => {
				decl::declare_constant(ctx, Some(scope), constdef);
			}
			ast::ClassInnard::States(states) => {
				declare_state_labels(ctx, sym_ix, scope, states);
			}
			ast::ClassInnard::StaticConst(sconst) => {
				decl::declare_static_const(ctx, scope, sconst);
			}
			ast::ClassInnard::Property(property) => {
				declare_property(ctx, sym_ix, scope, property);
			}
			ast::ClassInnard::Struct(structdef) => {
				decl::declare_struct(ctx, Some(scope), structdef);
			}
			ast::ClassInnard::Mixin(mixin_stat) => {
				expand_mixin(ctx, sym_ix, scope, mixin_stat);
			}
			ast::ClassInnard::Default(_) => continue,
		}
	}
}

pub(crate) fn declare_struct_innards(
	ctx: &FrontendContext,
	sym_ix: SymIx,
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
				declare_function(ctx, sym_ix, scope, fndecl, false);
			}
			ast::StructInnard::Field(field) => {
				declare_field(ctx, sym_ix, scope, field);
			}
		}
	}
}

#[must_use]
pub(crate) fn declare_mixin_class_innards(
	ctx: &FrontendContext,
	sym_ix: SymIx,
	ast: ast::MixinClassDef,
) -> Option<Scope> {
	let mut scope = Scope::default();

	for innard in ast.innards() {
		match innard {
			ast::ClassInnard::Const(constdef) => {
				decl::declare_constant(ctx, Some(&mut scope), constdef);
			}
			ast::ClassInnard::Enum(enumdef) => {
				decl::declare_enum(ctx, Some(&mut scope), enumdef);
			}
			ast::ClassInnard::Struct(structdef) => {
				decl::declare_struct(ctx, Some(&mut scope), structdef);
			}
			ast::ClassInnard::StaticConst(sconst) => {
				decl::declare_static_const(ctx, &mut scope, sconst);
			}
			ast::ClassInnard::Function(fndecl) => {
				declare_function(ctx, sym_ix, &mut scope, fndecl, true);
			}
			ast::ClassInnard::Field(field) => {
				declare_field(ctx, sym_ix, &mut scope, field);
			}
			ast::ClassInnard::States(states) => {
				declare_state_labels(ctx, sym_ix, &mut scope, states);
			}
			ast::ClassInnard::Property(property) => {
				declare_property(ctx, sym_ix, &mut scope, property);
			}
			ast::ClassInnard::Flag(flagdef) => {
				declare_flagdef(ctx, sym_ix, &mut scope, flagdef);
			}
			ast::ClassInnard::Default(_) => continue,
			ast::ClassInnard::Mixin(_) => unreachable!(),
		}
	}

	Some(scope)
}

pub(crate) fn extend_class(ctx: &FrontendContext, ast: ast::ClassExtend) {
	let name_tok = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&name_tok));

	let globals = ctx.global_scope(ctx.project_ix);

	let Some(&extended_ix) = globals.get(&ns_name) else {
		ctx.raise(ctx.src.diag_builder(
			name_tok.text_range(),
			DiagnosticSeverity::ERROR,
			format!("class `{}` not found", name_tok.text()),
		));

		return;
	};

	drop(globals);

	ctx.make_ref_to(name_tok.text_range(), extended_ix);

	let sym = match ctx.symbol(extended_ix) {
		OneOf::Left(u_sym) => u_sym,
		OneOf::Right(in_sym) => {
			if in_sym.lang != LangId::ZScript {
				// TODO: raise an issue.
				return;
			}

			ctx.raise(ctx.src.diag_builder(
				name_tok.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"internal symbol `{}` cannot be extended by user code",
					name_tok.text()
				),
			));

			return;
		}
	};

	if sym.lang != LangId::ZScript {
		// Only ZScript and DECORATE symbols can use `NsName::Type`,
		// so this is the only kind of error that's applicable here.
		ctx.raise(ctx.src.diag_builder(
			name_tok.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"DECORATE class `{}` cannot be extended from ZScript",
				name_tok.text()
			),
		));

		return;
	}

	match Syn::kind_from_raw(sym.syn) {
		Syn::ClassDef => {}
		Syn::EnumDef => {
			ctx.raise(ctx.src.diag_builder(
				name_tok.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"`extend class` not applicable to enum `{}`",
					name_tok.text()
				),
			));

			return;
		}
		Syn::StructDef => {
			ctx.raise(ctx.src.diag_builder(
				name_tok.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"`extend class` not applicable to struct `{}`",
					name_tok.text()
				),
			));

			return;
		}
		Syn::MixinClassDef => {
			ctx.raise(ctx.src.diag_builder(
				name_tok.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"`extend class` not applicable to mixin class `{}`",
					name_tok.text()
				),
			));

			return;
		}
		other => unreachable!("expected a symbol in the type namespace, found: {other:#?}"),
	}

	let mut scope = ctx.scopes.get_mut(&sym.id.0).unwrap_or_else(|| {
		let path = ctx.paths.resolve(sym.id.file_id);

		panic!(
			"ZScript class at {}:{:?} has no scope",
			path.display(),
			sym.id.span
		);
	});

	declare_class_innards(ctx, extended_ix, scope.value_mut(), ast.innards());
}

pub(crate) fn extend_struct(ctx: &FrontendContext, ast: ast::StructExtend) {
	let ident = ast.name().unwrap().into();
	let ns_name = NsName::Type(ctx.names.intern(&ident));

	let globals = ctx.global_scope(ctx.project_ix);

	let Some(&extended_ix) = globals.get(&ns_name) else {
		ctx.raise(ctx.src.diag_builder(
			ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!("struct `{}` not found", ident.text()),
		));

		return;
	};

	drop(globals);

	ctx.make_ref_to(ident.text_range(), extended_ix);

	let sym = match ctx.symbol(extended_ix) {
		OneOf::Left(u_sym) => u_sym,
		OneOf::Right(_in_sym) => {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!(
					"internal symbol `{}` cannot be extended by user code",
					ident.text()
				),
			));

			return;
		}
	};

	if sym.lang != LangId::ZScript {
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

	match Syn::kind_from_raw(sym.syn) {
		Syn::StructDef => {}
		Syn::ClassDef => {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!("`extend struct` not applicable to class `{}`", ident.text()),
			));

			return;
		}
		Syn::EnumDef => {
			ctx.raise(ctx.src.diag_builder(
				ident.text_range(),
				DiagnosticSeverity::ERROR,
				format!("`extend struct` not applicable to enum `{}`", ident.text()),
			));

			return;
		}
		Syn::MixinClassDef => {
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

	let mut scope = ctx.scopes.get_mut(&sym.id.0).unwrap();

	declare_struct_innards(ctx, extended_ix, scope.value_mut(), ast.innards());
}

fn expand_mixin(ctx: &FrontendContext, class_ix: SymIx, scope: &mut Scope, ast: ast::MixinStat) {
	let mixin_ident = ast.name().unwrap().into();
	let mixin_nsname = NsName::Type(ctx.names.intern(&mixin_ident));

	let globals = ctx.global_scope(ctx.project_ix);

	let Some(&mixin_ix) = globals.get(&mixin_nsname) else {
		ctx.raise(ctx.src.diag_builder(
			mixin_ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!("mixin class `{}` not found", mixin_ident.text()),
		));

		return;
	};

	drop(globals);

	ctx.make_ref_to(mixin_ident.text_range(), mixin_ix);

	if let Err(()) = ctx.make_mixin(class_ix, mixin_ix) {
		let OneOf::Left(cls_sym) = ctx.symbol(class_ix) else {
			unreachable!()
		};

		// TODO: should probably give symbols an `NsName` field...
		let cls_src = ctx.file_with(cls_sym);
		let cls_node = cls_src.node_covering::<Syn>(cls_sym.id.span);
		let class_def = ast::ClassDef::cast(cls_node).unwrap();

		ctx.raise(ctx.src.diag_builder(
			mixin_ident.text_range(),
			DiagnosticSeverity::ERROR,
			format!(
				"mixin class `{}` has already been applied to class `{}`",
				mixin_ident.text(),
				class_def.name().unwrap().text()
			),
		));

		return;
	}

	let OneOf::Left(mixin_sym) = ctx.symbol(mixin_ix) else {
		unimplemented!()
	};

	let new_ctx = FrontendContext {
		project_ix: mixin_sym.project as usize,
		src: ctx.file_with(mixin_sym),
		..*ctx
	};

	let node = new_ctx.src.node_covering::<Syn>(mixin_sym.id.span);
	let mixindef = ast::MixinClassDef::cast(node).unwrap();
	declare_class_innards(&new_ctx, class_ix, scope, mixindef.innards());
}

fn declare_field(ctx: &FrontendContext, holder: SymIx, outer: &mut Scope, ast: ast::FieldDecl) {
	for var_name in ast.names() {
		let name_tok = var_name.ident().into();
		let ns_name = NsName::Value(ctx.names.intern(&name_tok));

		let result = ctx.declare(
			outer,
			ns_name,
			LangId::ZScript,
			var_name.syntax(),
			var_name.syntax().text_range(),
		);

		match result {
			Ok(ix) => {
				ctx.make_member(ix, holder);
			}
			Err(prev) => {
				decl::redeclare_error(ctx, prev, var_name.syntax().text_range(), name_tok.text());
			}
		}
	}
}

fn declare_flagdef(ctx: &FrontendContext, holder: SymIx, outer: &mut Scope, ast: ast::FlagDef) {
	let name_tok = ast.name().unwrap().into();
	let crit_span = ast.syntax().text_range();

	let result = ctx.declare(
		outer,
		NsName::FlagDef(ctx.names.intern(&name_tok)),
		LangId::ZScript,
		ast.syntax(),
		crit_span,
	);

	match result {
		Ok(ix) => {
			ctx.make_member(ix, holder);
		}
		Err(prev) => {
			decl::redeclare_error(ctx, prev, crit_span, name_tok.text());
		}
	}
}

fn declare_function(
	ctx: &FrontendContext,
	holder: SymIx,
	outer: &mut Scope,
	ast: ast::FunctionDecl,
	class: bool,
) {
	let name_tok = ast.name().into();
	let ns_name = NsName::Value(ctx.names.intern(&name_tok));

	let crit_start = if let Some(qual) = ast.qualifiers().iter().next() {
		qual.text_range().start()
	} else {
		ast.return_types().syntax().text_range().start()
	};

	let crit_end = if let Some(kw) = ast.const_keyword() {
		kw.text_range().end()
	} else {
		ast.param_list().unwrap().syntax().text_range().end()
	};

	if !class {
		let result = ctx.declare(
			outer,
			ns_name,
			LangId::ZScript,
			ast.syntax(),
			TextRange::new(crit_start, crit_end),
		);

		match result {
			Ok(ix) => {
				ctx.make_member(ix, holder);
			}
			Err(prev) => {
				decl::redeclare_error(
					ctx,
					prev,
					TextRange::new(crit_start, crit_end),
					name_tok.text(),
				);
			}
		}
	} else {
		let (ix, overriden_opt) = ctx.decl_override(
			outer,
			ns_name,
			LangId::ZScript,
			ast.syntax(),
			TextRange::new(crit_start, crit_end),
		);

		ctx.make_member(ix, holder);

		if let Some(overriden) = overriden_opt {
			ctx.make_override_of(overriden, ix);
		}
	}
}

fn declare_property(
	ctx: &FrontendContext,
	holder: SymIx,
	outer: &mut Scope,
	ast: ast::PropertyDef,
) {
	let ident = ast.name().unwrap().into();
	let crit_span = ast.syntax().text_range();

	let result = ctx.declare(
		outer,
		NsName::Property(ctx.names.intern(&ident)),
		LangId::ZScript,
		ast.syntax(),
		crit_span,
	);

	match result {
		Ok(ix) => {
			ctx.make_member(ix, holder);
		}
		Err(prev) => {
			decl::redeclare_error(ctx, prev, crit_span, ident.text());
		}
	}
}

fn declare_state_labels(
	ctx: &FrontendContext,
	holder: SymIx,
	outer: &mut Scope,
	ast: ast::StatesBlock,
) {
	for innard in ast.innards() {
		let ast::StatesInnard::Label(label) = innard else {
			continue;
		};

		let name_tok = label.name().into();
		let ns_name = NsName::StateLabel(ctx.names.intern(&name_tok));

		let result = ctx.declare(
			outer,
			ns_name,
			LangId::ZScript,
			label.syntax(),
			label.syntax().text_range(),
		);

		match result {
			Ok(ix) => {
				ctx.make_member(ix, holder);
			}
			Err(prev) => {
				decl::redeclare_error(ctx, prev, label.syntax().text_range(), name_tok.text());
			}
		}
	}
}

// Details /////////////////////////////////////////////////////////////////////

/// If, for example, a class needs to inherit the scope of a parent, it "requires"
/// the parent class' scope. The thread trying to fill the child scope will check
/// if another thread has already done so, and use that scope if it exists. Otherwise,
/// that scope will get filled so it can be inherited from.
#[must_use]
fn require_scope(ctx: &FrontendContext, sym_ix: SymIx, sym: &Symbol) -> Option<Scope> {
	if sym.lang != LangId::ZScript {
		// TODO: raise an issue.
		return None;
	}

	let src = ctx.file_with(sym);

	let new_ctx = FrontendContext {
		src,
		project_ix: sym.project as usize,
		..*ctx
	};

	let file_node = SyntaxNode::new_root(src.green.as_ref().unwrap().clone());

	let sym_elem = file_node.covering_element(sym.id.span);

	let sym_node = match sym_elem {
		NodeOrToken::Node(n) => n,
		NodeOrToken::Token(t) => t.parent().unwrap(),
	};

	debug_assert_eq!(sym_node.kind(), Syn::kind_from_raw(sym.syn));

	let (sender, scope_opt) = match sym_node.kind() {
		Syn::ClassDef => {
			let sender = match ctx.get_scope_or_sender(sym.id.0) {
				OneOf::Left(sender) => sender,
				OneOf::Right(scope) => {
					return scope;
				}
			};

			let classdef = ast::ClassDef::cast(sym_node).unwrap();
			let scope_opt = declare_class_scope(&new_ctx, sym_ix, classdef);
			(sender, scope_opt)
		}
		Syn::StructDef => {
			let sender = match ctx.get_scope_or_sender(sym.id.0) {
				OneOf::Left(sender) => sender,
				OneOf::Right(scope) => {
					return scope;
				}
			};

			let structdef = ast::StructDef::cast(sym_node).unwrap();
			let mut scope = Scope::default();
			declare_struct_innards(&new_ctx, sym_ix, &mut scope, structdef.innards());
			(sender, Some(scope))
		}
		Syn::MixinClassDef => {
			let sender = match ctx.get_scope_or_sender(sym.id.0) {
				OneOf::Left(sender) => sender,
				OneOf::Right(scope) => {
					return scope;
				}
			};

			let mixindef = ast::MixinClassDef::cast(sym_node).unwrap();
			let scope_opt = declare_mixin_class_innards(&new_ctx, sym_ix, mixindef);
			(sender, scope_opt)
		}
		_ => return None,
	};

	let Some(scope) = scope_opt else {
		return None;
	};

	ctx.scopes.insert(sym.id.0, scope.clone());
	// If any other threads are waiting for this scope to be filled, service them.
	while let Ok(()) = sender.try_send(scope.clone()) {}

	Some(scope)
}
