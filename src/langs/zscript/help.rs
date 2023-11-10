//! Helper functions.

use doomfront::{
	rowan::{ast::AstNode, TextRange},
	zdoom::zscript::{ast, Syn, SyntaxNode},
};
use lsp_types::SymbolKind;
use vtutil::string::AnyDisplay;

use crate::{
	core::Core,
	data::{SymGraphKey, SymGraphVal, SymPtr, Symbol, UserSymbol},
	langs::zscript::sema::{self, ClassFlags, Datum, FieldFlags, StructFlags},
};

use super::sema::FunctionFlags;

/// Used for [`Core::decl_text`].
#[must_use]
pub(crate) fn decl_text(ctx: &Core, sym_ptr: &SymPtr, u_sym: &UserSymbol, datum: &Datum) -> String {
	use std::fmt::Write;

	let mut ret = String::new();

	match datum {
		Datum::Class(class_d) => {
			let _ = write!(ret, "class {}", ctx.names.resolve(u_sym.name));

			if let Some(sgn) = ctx
				.ready
				.sym_graph
				.get(&SymGraphKey::ParentOf(sym_ptr.clone()))
			{
				let parent_sym = sgn.as_symbol().unwrap();

				match parent_sym.as_ref().unwrap() {
					Symbol::User(parent_u) => {
						let _ = write!(ret, " : {}", ctx.names.resolve(parent_u.name));
					}
					Symbol::Internal(parent_in) => {
						let _ = write!(ret, " : {}", parent_in.name);
					}
				}
			}

			if class_d.flags.contains(ClassFlags::NATIVE) {
				let _ = write!(ret, " native");
			}

			if class_d.flags.contains(ClassFlags::ABSTRACT) {
				let _ = write!(ret, " abstract");
			}

			match class_d.scope {
				sema::Scope::Data => {}
				sema::Scope::Ui => {
					let _ = write!(ret, " ui");
				}
				sema::Scope::Play => {
					let _ = write!(ret, " play");
				}
				sema::Scope::Virtual => unreachable!(),
			}

			if let Some(v) = class_d.min_version {
				let _ = write!(ret, " version(\"{v}\")\r\n");
			}
		}
		Datum::Constant => {
			// TODO
			let _ = write!(ret, "const {} = <error>", ctx.names.resolve(u_sym.name));
		}
		Datum::Enum(enum_d) => {
			let _ = write!(
				ret,
				"enum {} : {}",
				ctx.names.resolve(u_sym.name),
				enum_d.underlying
			);
		}
		Datum::Field(field_d) => {
			if field_d.flags.contains(FieldFlags::NATIVE) {
				let _ = write!(ret, "native ");
			}

			let _ = write!(ret, "{}", ctx.names.resolve(u_sym.name));
		}
		Datum::Function(fn_d) => {
			match fn_d.vis {
				sema::Visibility::Protected => {
					let _ = write!(ret, "protected ");
				}
				sema::Visibility::Private => {
					let _ = write!(ret, "private ");
				}
				sema::Visibility::Public => {}
			}

			if fn_d.flags.contains(FunctionFlags::ABSTRACT) {
				let _ = write!(ret, "abstract ");
			}

			if fn_d.flags.contains(FunctionFlags::FINAL) {
				let _ = write!(ret, "final ");
			}

			if fn_d.flags.contains(FunctionFlags::NATIVE) {
				let _ = write!(ret, "native ");
			}

			if fn_d.flags.contains(FunctionFlags::OVERRIDE) {
				let _ = write!(ret, "override ");
			}

			if fn_d.flags.contains(FunctionFlags::STATIC) {
				let _ = write!(ret, "static ");
			}

			if fn_d.flags.contains(FunctionFlags::VARARGS) {
				let _ = write!(ret, "varArg ");
			}

			if fn_d.flags.contains(FunctionFlags::VIRTUAL) {
				let _ = write!(ret, "virtual ");
			}

			if fn_d.flags.contains(FunctionFlags::ACTION) {
				let _ = write!(ret, "action");

				if fn_d.flags.contains(
					FunctionFlags::ACTION_ACTOR
						| FunctionFlags::ACTION_ITEM
						| FunctionFlags::ACTION_OVERLAY
						| FunctionFlags::ACTION_WEAPON,
				) {
					let _ = write!(ret, "(");

					if fn_d.flags.contains(FunctionFlags::ACTION_ACTOR) {
						let _ = write!(ret, "actor, ");
					}

					if fn_d.flags.contains(FunctionFlags::ACTION_ITEM) {
						let _ = write!(ret, "item, ");
					}

					if fn_d.flags.contains(FunctionFlags::ACTION_OVERLAY) {
						let _ = write!(ret, "overlay, ");
					}

					if fn_d.flags.contains(FunctionFlags::ACTION_WEAPON) {
						let _ = write!(ret, "weapon, ");
					}

					ret.truncate(ret.len() - 2);
					let _ = write!(ret, ")");
				}

				let _ = write!(ret, " ");
			}

			if let Some(sgv) = ctx
				.ready
				.sym_graph
				.get(&SymGraphKey::Holder(sym_ptr.clone()))
			{
				let SymGraphVal::Symbol(holder) = sgv else {
					unreachable!()
				};

				let holder_name = ctx.names.resolve(holder.as_user().unwrap().name);
				let _ = write!(ret, "{holder_name}.");
			}

			let _ = write!(ret, "{}(", ctx.names.resolve(u_sym.name));

			for param in &fn_d.params {
				if param.in_ref {
					let _ = write!(ret, "in ");
				}

				if param.out_ref {
					let _ = write!(ret, "out ");
				}

				let pname = ctx.names.resolve(param.name);
				let formatter = AnyDisplay(&param.qtype, ctx, sema::QualType::format);
				let _ = write!(ret, "{formatter} {pname}, ");
			}

			ret.truncate(ret.len() - 2);
			let _ = write!(ret, ")");

			if fn_d.flags.contains(FunctionFlags::CONST) {
				let _ = write!(ret, " const");
			}
		}
		Datum::_MixinClass => {
			let _ = write!(ret, "mixin class {}", ctx.names.resolve(u_sym.name));
		}
		Datum::Primitive(_) => {
			let _ = write!(ret, "{}", ctx.names.resolve(u_sym.name));
		}
		Datum::Struct(struct_d) => {
			let _ = write!(ret, "struct {}", ctx.names.resolve(u_sym.name));

			if struct_d.flags.contains(StructFlags::NATIVE) {
				let _ = write!(ret, " native");
			}

			match struct_d.scope {
				sema::Scope::Data => {}
				sema::Scope::Ui => {
					let _ = write!(ret, " ui");
				}
				sema::Scope::Play => {
					let _ = write!(ret, " play");
				}
				sema::Scope::Virtual => unreachable!(),
			}

			if let Some(v) = struct_d.min_version {
				let _ = write!(ret, " version(\"{v}\")\r\n");
			}
		}
	}

	ret
}

#[must_use]
pub(crate) fn lsp_kind(_: &UserSymbol, datum: &Datum) -> SymbolKind {
	match datum {
		Datum::Class(_) => SymbolKind::CLASS,
		Datum::Constant => SymbolKind::CONSTANT, // TODO: check if enum variant.
		Datum::Enum(_) => SymbolKind::ENUM,
		Datum::Field(_) => SymbolKind::FIELD,
		Datum::Function(fn_d) => {
			if fn_d.flags.contains(FunctionFlags::STATIC) {
				SymbolKind::FUNCTION
			} else {
				SymbolKind::METHOD
			}
		}
		Datum::_MixinClass => SymbolKind::INTERFACE,
		Datum::Primitive(_) => SymbolKind::OPERATOR, // Strictly speaking, this is unreachable.
		Datum::Struct(_) => SymbolKind::STRUCT,
	}
}

/// Used for [`FrontendContext::diag_location`].
#[must_use]
pub(crate) fn symbol_crit_span(node: &SyntaxNode) -> TextRange {
	match node.kind() {
		Syn::ClassDef => {
			let classdef = ast::ClassDef::cast(node.clone()).unwrap();
			let start = classdef.keyword().text_range().start();

			let end = if let Some(qual) = classdef.qualifiers().last() {
				qual.text_range().end()
			} else if let Some(parent) = classdef.parent_class() {
				parent.text_range().end()
			} else {
				classdef.name().unwrap().text_range().end()
			};

			TextRange::new(start, end)
		}
		Syn::FunctionDecl => {
			let fndecl = ast::FunctionDecl::cast(node.clone()).unwrap();

			let start = if let Some(qual) = fndecl.qualifiers().iter().next() {
				qual.text_range().start()
			} else {
				fndecl.return_types().syntax().text_range().start()
			};

			let end = if let Some(kw) = fndecl.const_keyword() {
				kw.text_range().end()
			} else {
				fndecl.param_list().unwrap().syntax().text_range().end()
			};

			TextRange::new(start, end)
		}
		Syn::VarName => {
			let parent = node.parent().unwrap();
			debug_assert_eq!(parent.kind(), Syn::FieldDecl);
			parent.text_range()
		}
		Syn::StateLabel | Syn::FlagDef | Syn::PropertyDef | Syn::EnumVariant => node.text_range(),
		Syn::StructDef => {
			let structdef = ast::StructDef::cast(node.clone()).unwrap();

			let start = structdef.keyword().text_range().start();

			let end = if let Some(qual) = structdef.qualifiers().last() {
				qual.text_range().end()
			} else {
				structdef.name().unwrap().text_range().end()
			};

			TextRange::new(start, end)
		}
		Syn::StaticConstStat => {
			let sconst = ast::StaticConstStat::cast(node.clone()).unwrap();

			TextRange::new(
				sconst.keywords().0.text_range().start(),
				sconst.name().unwrap().text_range().end(),
			)
		}
		Syn::MixinClassDef => {
			let mixindef = ast::MixinClassDef::cast(node.clone()).unwrap();
			let ident = mixindef.name().unwrap();

			TextRange::new(
				mixindef.keywords().0.text_range().start(),
				ident.text_range().start(),
			)
		}
		Syn::ConstDef => {
			let constdef = ast::ConstDef::cast(node.clone()).unwrap();

			TextRange::new(
				constdef.keyword().text_range().start(),
				constdef.syntax().text_range().end(),
			)
		}
		Syn::EnumDef => {
			let enumdef = ast::EnumDef::cast(node.clone()).unwrap();
			let ident = enumdef.name().unwrap();

			let start = enumdef.keyword().text_range().start();

			let end = if let Some(tspec) = enumdef.type_spec() {
				tspec.0.text_range().end()
			} else {
				ident.text_range().end()
			};

			TextRange::new(start, end)
		}
		other => unreachable!("called `symbol_crit_span` on non-symbol node: {other:#?}"),
	}
}
