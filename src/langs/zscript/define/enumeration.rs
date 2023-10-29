use doomfront::zdoom::zscript::ast;

use crate::{data::SymPtr, frontend::FrontendContext};

pub(crate) fn define(ctx: &FrontendContext, enum_ptr: SymPtr, ast: ast::EnumDef) {
	ctx.make_ref_to(ast.name().unwrap().text_range(), enum_ptr);
}
