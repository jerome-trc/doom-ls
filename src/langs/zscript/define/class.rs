use doomfront::{rowan::ast::AstNode, zdoom::zscript::ast};

use crate::{
	arena::Arena,
	data::{Definition, SymPtr},
	frontend::FrontendContext,
	langs::zscript::sema::Datum,
};

use super::function;

pub(crate) fn define(ctx: &FrontendContext, class_ptr: SymPtr, ast: ast::ClassDef) {
	let datum = Datum::Class;

	ctx.make_ref_to(ast.name().unwrap().text_range(), class_ptr.clone());

	if let Some(parent_ident) = ast.parent_class() {
		if let Some(parent_ref) = ctx.parent_of(class_ptr.clone()) {
			let parent_ptr = parent_ref.as_symbol().unwrap();
			ctx.make_ref_to(parent_ident.text_range(), parent_ptr.clone());
		}
	}

	for innard in ast.innards() {
		match innard {
			ast::ClassInnard::Function(fndecl) => {
				let sym_ptr = ctx
					.get_symbol(ctx.src, fndecl.syntax().text_range())
					.unwrap();
				function::define(ctx, sym_ptr, fndecl, true);
			}
			ast::ClassInnard::Const(_)
			| ast::ClassInnard::Enum(_)
			| ast::ClassInnard::Struct(_)
			| ast::ClassInnard::StaticConst(_)
			| ast::ClassInnard::Field(_)
			| ast::ClassInnard::Mixin(_)
			| ast::ClassInnard::Default(_)
			| ast::ClassInnard::States(_)
			| ast::ClassInnard::Property(_)
			| ast::ClassInnard::Flag(_) => {} // TODO
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
				function::define(ctx, sym_ptr, fndecl, true);
			}
			ast::ClassInnard::Const(_)
			| ast::ClassInnard::Enum(_)
			| ast::ClassInnard::Struct(_)
			| ast::ClassInnard::StaticConst(_)
			| ast::ClassInnard::Field(_)
			| ast::ClassInnard::Mixin(_)
			| ast::ClassInnard::Default(_)
			| ast::ClassInnard::States(_)
			| ast::ClassInnard::Property(_)
			| ast::ClassInnard::Flag(_) => {} // TODO
		}
	}
}
