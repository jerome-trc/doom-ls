use doomfront::{rowan::ast::AstNode, zdoom::zscript::ast};

use crate::{
	arena::Arena,
	data::{Definition, SymPtr},
	frontend::FrontendContext,
	langs::zscript::sema::{self, ClassFlags, Datum},
};

pub(crate) fn define(ctx: &FrontendContext, class_ptr: SymPtr, ast: ast::ClassDef) {
	let datum = Datum::Class(sema::Class {
		flags: ClassFlags::empty(),
		scope: sema::Scope::Data, // TODO: inherit from parent.
		min_version: None,
	});

	ctx.make_ref_to(ast.head().name().unwrap().text_range(), class_ptr.clone());

	if let Some(parent_ident) = ast.head().parent_class() {
		if let Some(parent_ref) = ctx.parent_of(class_ptr.clone()) {
			let parent_ptr = parent_ref.as_symbol().unwrap().clone();
			drop(parent_ref);
			ctx.make_ref_to(parent_ident.text_range(), parent_ptr);
		}
	}

	for innard in ast.innards() {
		match innard {
			ast::ClassInnard::Function(fndecl) => {
				let sym_ptr = ctx
					.get_symbol(ctx.src, fndecl.syntax().text_range())
					.unwrap();
				super::function::define(ctx, sym_ptr, fndecl, true);
			}
			ast::ClassInnard::Field(field) => {
				let base = super::field::Base::new(ctx, field);

				for varname in base.ast.names() {
					let sym_ptr = ctx
						.get_symbol(ctx.src, varname.syntax().text_range())
						.unwrap();

					super::field::define(ctx, sym_ptr, varname, &base, true);
				}
			}
			ast::ClassInnard::Const(_)
			| ast::ClassInnard::Enum(_)
			| ast::ClassInnard::Struct(_)
			| ast::ClassInnard::StaticConst(_)
			| ast::ClassInnard::Default(_)
			| ast::ClassInnard::States(_)
			| ast::ClassInnard::Property(_)
			| ast::ClassInnard::Flag(_) => {} // TODO
			ast::ClassInnard::Mixin(_) => continue,
		}
	}

	let mut bump = ctx.arena.borrow();
	let def_ptr = Arena::alloc(&mut bump, Definition::ZScript(datum));
	class_ptr
		.as_user()
		.unwrap()
		.def
		.store(def_ptr.as_ptr().unwrap());
}

pub(crate) fn extend(ctx: &FrontendContext, class_ptr: SymPtr, ast: ast::ClassExtend) {
	ctx.make_ref_to(ast.name().unwrap().text_range(), class_ptr.clone());

	for innard in ast.innards() {
		match innard {
			ast::ClassInnard::Function(fndecl) => {
				let sym_ptr = ctx
					.get_symbol(ctx.src, fndecl.syntax().text_range())
					.unwrap();
				super::function::define(ctx, sym_ptr, fndecl, true);
			}
			ast::ClassInnard::Field(field) => {
				let base = super::field::Base::new(ctx, field);

				for varname in base.ast.names() {
					let sym_ptr = ctx
						.get_symbol(ctx.src, varname.syntax().text_range())
						.unwrap();

					super::field::define(ctx, sym_ptr, varname, &base, true);
				}
			}
			ast::ClassInnard::Const(_)
			| ast::ClassInnard::Enum(_)
			| ast::ClassInnard::Struct(_)
			| ast::ClassInnard::StaticConst(_)
			| ast::ClassInnard::Mixin(_)
			| ast::ClassInnard::Default(_)
			| ast::ClassInnard::States(_)
			| ast::ClassInnard::Property(_)
			| ast::ClassInnard::Flag(_) => {} // TODO
		}
	}
}
