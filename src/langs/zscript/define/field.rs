use doomfront::{
	rowan::{ast::AstNode, TextRange},
	zdoom::zscript::ast::{self, MemberQualSet},
};
use lsp_types::{DiagnosticRelatedInformation, DiagnosticSeverity};

use crate::{
	arena::Arena,
	data::{Definition, SymPtr},
	frontend::FrontendContext,
	langs::zscript::sema::{self, Datum, FieldFlags},
	util::DiagBuilder,
};

#[derive(Debug)]
pub(crate) struct Base {
	pub(crate) ast: ast::FieldDecl,
	pub(crate) qualset: MemberQualSet,
	pub(crate) type_spec: ast::TypeRef,
}

impl Base {
	#[must_use]
	pub(crate) fn new(ctx: &FrontendContext, ast: ast::FieldDecl) -> Self {
		#[must_use]
		fn pfx_init(ast: &ast::FieldDecl, name_count: usize) -> String {
			use std::fmt::Write;

			let mut pfx = String::new();

			if name_count == 1 {
				let _ = write!(
					pfx,
					"field `{}` has",
					ast.names().next().unwrap().ident().text()
				);
			} else {
				let _ = write!(pfx, "fields ");

				for varname in ast.names().take(name_count - 1) {
					let _ = write!(pfx, "`{}`, ", varname.ident().text());
				}

				let _ = write!(
					pfx,
					"and `{}` have",
					ast.names().last().unwrap().ident().text()
				);
			}

			pfx
		}

		#[must_use]
		fn qual_redundancy_lint(
			ctx: &FrontendContext,
			prev_span: TextRange,
			repeat_span: TextRange,
			pfx: &str,
			kw: &'static str,
		) -> DiagBuilder {
			debug_assert!(!pfx.is_empty());

			ctx.src
				.diag_builder(
					repeat_span,
					DiagnosticSeverity::WARNING,
					format!("{pfx} multiple `{kw}` qualifiers"),
				)
				.with_related(DiagnosticRelatedInformation {
					location: ctx.make_location(ctx.src, prev_span),
					message: "`{kw}` qualifier previously applied here is redundant".to_string(),
				})
		}

		let quals = ast.qualifiers();
		let type_spec = ast.type_spec().unwrap();

		let mut diag_pfx = String::new();

		let qualset =
			ast::MemberQualSet::new(&quals, |prev_span, repeated| {
				if diag_pfx.is_empty() {
					diag_pfx = pfx_init(&ast, ast.names().count());
				}

				match repeated {
					ast::MemberQual::Action(inner) => {
						ctx.raise(
							ctx.src
								.diag_builder(
									inner.syntax().text_range(),
									DiagnosticSeverity::WARNING,
									format!("{diag_pfx} multiple `action` qualifiers"),
								)
								.with_related(DiagnosticRelatedInformation {
									location: ctx.make_location(ctx.src, prev_span),
									message:
										"`action` qualifier previously applied here will be ignored"
											.to_string(),
								}),
						);
					}
					ast::MemberQual::Deprecation(inner) => {
						ctx.raise(ctx.src.diag_builder(
						inner.syntax().text_range(),
						DiagnosticSeverity::WARNING,
						format!("{diag_pfx} multiple `deprecated` qualifiers"),
						).with_related(DiagnosticRelatedInformation {
							location: ctx.make_location(ctx.src, prev_span),
							message: "`deprecated` qualifier previously applied here will be ignored".to_string()
						}));
					}
					ast::MemberQual::Version(inner) => {
						ctx.raise(
							ctx.src
								.diag_builder(
									inner.syntax().text_range(),
									DiagnosticSeverity::WARNING,
									format!("{diag_pfx} multiple `version` qualifiers"),
								)
								.with_related(DiagnosticRelatedInformation {
									location: ctx.make_location(ctx.src, prev_span),
									message:
										"`version` qualifier previously applied here will be ignored"
											.to_string(),
								}),
						);
					}
					ast::MemberQual::ClearScope(inner) => {
						ctx.raise(qual_redundancy_lint(
							ctx,
							prev_span,
							inner.text_range(),
							&diag_pfx,
							"`clearScope`",
						));
					}
					ast::MemberQual::Meta(inner) => {
						ctx.raise(qual_redundancy_lint(
							ctx,
							prev_span,
							inner.text_range(),
							&diag_pfx,
							"`meta`",
						));
					}
					ast::MemberQual::ReadOnly(inner) => {
						ctx.raise(qual_redundancy_lint(
							ctx,
							prev_span,
							inner.text_range(),
							&diag_pfx,
							"`readonly`",
						));
					}
					ast::MemberQual::Transient(inner) => {
						ctx.raise(qual_redundancy_lint(
							ctx,
							prev_span,
							inner.text_range(),
							&diag_pfx,
							"`transient`",
						));
					}
					// Scope qualifiers get processed later.
					ast::MemberQual::Play(_) | ast::MemberQual::Ui(_) => {}
					// Visibility qualifiers get processed later.
					ast::MemberQual::Private(_) | ast::MemberQual::Protected(_) => {}
					// Useless to user code. Diagnostics get raised about these later.
					ast::MemberQual::Internal(_) | ast::MemberQual::Native(_) => {}
					// Inapplicable to fields. Diagnostics get raised about these later.
					ast::MemberQual::Abstract(_)
					| ast::MemberQual::Final(_)
					| ast::MemberQual::Override(_)
					| ast::MemberQual::Static(_)
					| ast::MemberQual::VarArg(_)
					| ast::MemberQual::Virtual(_)
					| ast::MemberQual::VirtualScope(_) => {}
				}
			});

		Self {
			ast,
			qualset,
			type_spec,
		}
	}
}

pub(crate) fn define(
	ctx: &FrontendContext,
	field_ptr: SymPtr,
	ast: ast::VarName,
	base: &Base,
	class_member: bool,
) {
	ctx.make_ref_to(ast.ident().text_range(), field_ptr.clone());

	let mut datum = sema::Field {
		flags: FieldFlags::empty(),
		scope: sema::Scope::Data, // TODO: inherit from holder.
		deprecated: None,
	};

	process_qualifiers(ctx, base, &mut datum);

	let mut bump = ctx.arena.borrow();
	let def_ptr = Arena::alloc(&mut bump, Definition::ZScript(Datum::Field(datum)));
	field_ptr
		.as_user()
		.unwrap()
		.def
		.store(def_ptr.as_ptr().unwrap());
}

fn process_qualifiers(ctx: &FrontendContext, base: &Base, datum: &mut sema::Field) {
	for qual in base.ast.qualifiers().iter() {
		match qual {
			ast::MemberQual::Action(_) => todo!(),
			ast::MemberQual::Deprecation(_) => todo!(),
			ast::MemberQual::Version(_) => todo!(),
			ast::MemberQual::Abstract(_) => todo!(),
			ast::MemberQual::ClearScope(_) => todo!(),
			ast::MemberQual::Final(_) => todo!(),
			ast::MemberQual::Internal(_) => todo!(),
			ast::MemberQual::Meta(_) => todo!(),
			ast::MemberQual::Native(_) => todo!(),
			ast::MemberQual::Override(_) => todo!(),
			ast::MemberQual::Play(_) => todo!(),
			ast::MemberQual::Private(_) => todo!(),
			ast::MemberQual::Protected(_) => todo!(),
			ast::MemberQual::ReadOnly(_) => todo!(),
			ast::MemberQual::Static(_) => todo!(),
			ast::MemberQual::Transient(_) => todo!(),
			ast::MemberQual::Ui(_) => todo!(),
			ast::MemberQual::VarArg(_) => todo!(),
			ast::MemberQual::Virtual(_) => todo!(),
			ast::MemberQual::VirtualScope(_) => todo!(),
		}
	}
}
