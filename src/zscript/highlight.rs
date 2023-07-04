//! Semantic token highlighting.

use doomfront::{
	rowan::ast::AstNode,
	zdoom::zscript::{ast, Syn, SyntaxElem, SyntaxNode, SyntaxToken},
};

use crate::semtokens::{Highlighter, SemToken};

pub(super) fn walk_tree(hl: &mut Highlighter, cursor: SyntaxNode) {
	for elem in cursor.children_with_tokens() {
		match elem {
			SyntaxElem::Node(node) => {
				let Some(top) = ast::TopLevel::cast(node) else { continue; };

				match top {
					ast::TopLevel::ClassDef(_) => {}
					ast::TopLevel::ClassExtend(_) => {}
					ast::TopLevel::ConstDef(constdef) => {
						highlight_constdef(hl, constdef);
					}
					ast::TopLevel::EnumDef(_) => {}
					ast::TopLevel::MixinClassDef(_) => {}
					ast::TopLevel::Include(include) => {
						highlight_include_directive(hl, include);
					}
					ast::TopLevel::StructDef(_) => {}
					ast::TopLevel::StructExtend(_) => {}
					ast::TopLevel::Version(version) => {
						highlight_version_directive(hl, version);
					}
				}
			}
			SyntaxElem::Token(token) => {
				highlight_toplevel_token(hl, token);
			}
		}
	}
}

fn highlight_toplevel_token(hl: &mut Highlighter, token: SyntaxToken) {
	if token.kind().is_glyph() || token.kind().is_keyword() {
		// None of these are allowed at the top level.
		return;
	}

	let semtoken = match token.kind() {
		// Legal
		Syn::Comment | Syn::DocComment => SemToken::Comment,
		// Illegal
		Syn::IntLit | Syn::FloatLit => SemToken::Number,
		Syn::StringLit | Syn::NameLit => SemToken::String,
		Syn::NullLit => SemToken::Keyword,
		Syn::Ident => SemToken::Variable,
		// Likely other trivia
		_ => return,
	};

	hl.advance(semtoken, token.text_range());
}

fn highlight_constdef(hl: &mut Highlighter, constdef: ast::ConstDef) {
	for elem in constdef.syntax().children_with_tokens() {
		match elem {
			SyntaxElem::Token(token) => {
				if token.kind().is_keyword() {
					hl.advance(SemToken::Keyword, token.text_range());
				} else if token.kind() == Syn::Ident {
					hl.advance(SemToken::Type, token.text_range());
				} else if matches!(token.kind(), Syn::Comment | Syn::DocComment) {
					hl.advance(SemToken::Comment, token.text_range());
				}
			}
			SyntaxElem::Node(node) => {
				let Some(expr) = ast::Expr::cast(node) else { continue; };
				highlight_expr(hl, expr);
			}
		}
	}
}

fn highlight_include_directive(hl: &mut Highlighter, include: ast::IncludeDirective) {
	for elem in include.syntax().children_with_tokens() {
		match elem {
			SyntaxElem::Token(token) => match token.kind() {
				Syn::StringLit => hl.advance(SemToken::String, token.text_range()),
				Syn::PoundInclude => hl.advance(SemToken::Keyword, token.text_range()),
				Syn::Comment | Syn::DocComment => hl.advance(SemToken::Comment, token.text_range()),
				_ => {}
			},
			SyntaxElem::Node(node) => {
				highlight_error_node(hl, node);
			}
		}
	}
}

fn highlight_version_directive(hl: &mut Highlighter, version: ast::VersionDirective) {
	for elem in version.syntax().children_with_tokens() {
		match elem {
			SyntaxElem::Token(token) => match token.kind() {
				Syn::StringLit => hl.advance(SemToken::String, token.text_range()),
				Syn::KwVersion => hl.advance(SemToken::Keyword, token.text_range()),
				Syn::Comment | Syn::DocComment => hl.advance(SemToken::Comment, token.text_range()),
				_ => {}
			},
			SyntaxElem::Node(node) => {
				highlight_error_node(hl, node);
			}
		}
	}
}

fn highlight_expr(hl: &mut Highlighter, expr: ast::Expr) {
	match expr {
		ast::Expr::Binary(e_bin) => {
			for elem in e_bin.syntax().children_with_tokens() {
				match elem {
					SyntaxElem::Node(node) => {
						if ast::Expr::can_cast(node.kind()) {
							highlight_expr(hl, ast::Expr::cast(node).unwrap());
						} else {
							highlight_error_node(hl, node);
						}
					}
					SyntaxElem::Token(token) => {
						if token.kind().is_glyph() {
							hl.advance(SemToken::Operator, token.text_range());
						} else if matches!(token.kind(), Syn::Comment | Syn::DocComment) {
							hl.advance(SemToken::Comment, token.text_range());
						}
					}
				}
			}
		}
		ast::Expr::Call(_) => {}
		ast::Expr::ClassCast(_) => {}
		ast::Expr::Group(e_grp) => {
			highlight_expr(hl, e_grp.inner());
		}
		ast::Expr::Ident(_) => {}
		ast::Expr::Index(_) => {}
		ast::Expr::Literal(e_lit) => {
			if let Some(strings) = e_lit.strings() {
				for token in strings {
					hl.advance(SemToken::String, token.syntax().text_range());
				}
			} else {
				let lit = e_lit.token();

				if lit.bool().is_some() || lit.null() {
					hl.advance(SemToken::Keyword, lit.syntax().text_range());
				} else if lit.string().is_some() || lit.name().is_some() {
					hl.advance(SemToken::String, lit.syntax().text_range());
				} else if lit.int().is_some() || lit.float().is_some() {
					hl.advance(SemToken::Number, lit.syntax().text_range());
				}
			}
		}
		ast::Expr::Postfix(_) => {}
		ast::Expr::Prefix(_) => {}
		ast::Expr::Super(e_super) => {
			hl.advance(SemToken::Keyword, e_super.token().text_range());
		}
		ast::Expr::Ternary(_) => {}
		ast::Expr::Vector(_) => {}
	}
}

fn highlight_error_node(hl: &mut Highlighter, node: SyntaxNode) {
	debug_assert_eq!(node.kind(), Syn::Error);

	for elem in node.children_with_tokens() {
		let SyntaxElem::Token(token) = elem else { unreachable!(); };

		if matches!(token.kind(), Syn::Comment | Syn::DocComment) {
			hl.advance(SemToken::Comment, token.text_range());
		}
	}
}

#[cfg(test)]
mod test {
	use doomfront::zdoom::zscript::ParseTree;

	use super::*;

	#[test]
	fn smoke() {
		const SOURCE: &str = r##""##; // TODO: need some test data...

		let newlines = crate::scan::compute_newlines(SOURCE).into();
		let mut hl = Highlighter::new(newlines);

		let ptree: ParseTree = doomfront::parse(
			SOURCE,
			doomfront::zdoom::zscript::parse::file,
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		);

		walk_tree(&mut hl, ptree.cursor());
		dbg!(hl.tokens);
	}
}
