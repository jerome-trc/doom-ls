use doomfront::zdoom::zscript::ast;

use crate::{data::SymPtr, frontend::FrontendContext};

pub(crate) fn define(ctx: &FrontendContext, struct_ptr: SymPtr, ast: ast::StructDef) {
	ctx.make_ref_to(ast.name().unwrap().text_range(), struct_ptr);
}
