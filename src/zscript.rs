//! Infrastructure related to the [ZScript](doomfront::zdoom::zscript) language.

mod data;
mod highlight;

use std::{
	ops::ControlFlow,
	path::{Path, PathBuf},
	sync::Arc,
};

use doomfront::{
	rowan::{ast::AstNode, GreenNode, NodeOrToken, TextRange, TextSize},
	zdoom::zscript::{ast, Syn, SyntaxNode},
};
use lsp_server::{Connection, Message, RequestId, Response};
use lsp_types::{
	Diagnostic, DiagnosticSeverity, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
	LanguageString, MarkedString, Position, SemanticTokens, SemanticTokensRangeResult,
	SemanticTokensResult,
};
use parking_lot::Mutex;
use rayon::prelude::*;
use tracing::warn;

use crate::{
	lines::{LineCol, LineIndex},
	project::{self, Compiler, FileId, FilePos, GreenFile, Project, QName, SymbolTable},
	semtokens::Highlighter,
	util,
	zpath::{ZPath, ZPathBuf},
	Core, ErrorBox, FxIndexSet, LangId, MsgError, UnitResult,
};

pub(crate) use self::data::*;

#[salsa::query_group(FrontendDatabase)]
pub(crate) trait Frontend {
	#[salsa::interned]
	fn zscript_class(&self, datum: Arc<ClassDatum>) -> ClassKey;

	#[salsa::interned]
	fn zscript_constant(&self, datum: Arc<ConstantDatum>) -> ConstantKey;

	#[salsa::interned]
	fn zscript_enum(&self, datum: Arc<EnumDatum>) -> EnumKey;

	#[salsa::interned]
	fn zscript_mixin_class(&self, datum: Arc<MixinClassDatum>) -> MixinClassKey;

	#[salsa::interned]
	fn zscript_struct(&self, datum: Arc<StructDatum>) -> StructKey;

	fn zscript_symbol_table(&self, file_id: FileId, gfile: GreenFile) -> SymbolTable;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SymbolKey {
	Class(ClassKey),
	Constant(ConstantKey),
	Enum(EnumKey),
	Mixin(MixinClassKey),
	Struct(StructKey),
}

impl Core {
	pub(super) fn zscript_req_goto(
		&mut self,
		conn: &Connection,
		gfile: GreenFile,
		id: RequestId,
		position: Position,
		ix_project: usize,
	) -> UnitResult {
		let cursor = SyntaxNode::new_root(gfile.root);

		let linecol = LineCol {
			line: position.line,
			col: position.character,
		};

		let Some(boffs) = gfile.lndx.offset(linecol) else {
			Self::respond_null(conn, id)?;
			return Err(
				Box::new(MsgError(
					format!("failed to get token at position {linecol:#?}")
				))
			);
		};

		let Some(token) = cursor.token_at_offset(boffs).next() else {
			Self::respond_null(conn, id)?;
			return Err(Box::new(MsgError(
				format!("failed to get token at position {linecol:#?}")
			)));
		};

		if token.kind() != Syn::Ident {
			// For now, only identifiable things can have "definitions".
			// TODO: string and name literals can point to all kinds of other things.
			Self::respond_null(conn, id)?;
			return Ok(());
		}

		let parent = token.parent().unwrap();

		if token.text().eq_ignore_ascii_case("self") && ast::IdentExpr::can_cast(parent.kind()) {
			// No useful information to provide here.
			Self::respond_null(conn, id)?;
			return Ok(());
		}

		// TODO: If the user put in a "go to definition" request on the identifier
		// making up the declaration, respond with a list of references.

		for i in (0..=ix_project).rev() {
			match self.projects[i].zscript_req_goto_ident(token.text()) {
				ControlFlow::Continue(()) => {}
				ControlFlow::Break(Err(err)) => {
					return Err(err);
				}
				ControlFlow::Break(Ok(resp)) => {
					conn.sender.send(Message::Response(Response {
						id,
						result: Some(serde_json::to_value(resp).unwrap()),
						error: None,
					}))?;

					return Ok(());
				}
			}
		}

		Self::respond_null(conn, id)?;
		Ok(())
	}

	pub(super) fn zscript_req_hover(
		&self,
		conn: &Connection,
		gfile: GreenFile,
		id: RequestId,
		params: HoverParams,
	) -> UnitResult {
		let pos = params.text_document_position_params.position;

		let Some(token) = gfile.token_at::<Syn>(pos) else {
			warn!("Failed to find token specified by a hover request.");

			conn.sender.send(Message::Response(
				Response {
					id,
					result: None,
					error: None, // TODO
				}
			))?;

			return Ok(());
		};

		let contents = match token.kind() {
			Syn::KwClass => {
				HoverContents::Array(vec![
					MarkedString::LanguageString(LanguageString {
						language: "zscript".to_string(),
						value: "class".to_string(),
					}),
					MarkedString::String("A class defines an object type within ZScript, and is most of what you'll be creating within the language.".to_string())
					])
			}
			Syn::KwStruct => {
				HoverContents::Array(vec![
					MarkedString::LanguageString(LanguageString {
						language: "zscript".to_string(),
						value: "struct".to_string(),
					}),
					MarkedString::String("A structure is an object type that does not inherit from Object and is not always — though occasionally is — a reference type, unlike classes.".to_string())
					])
			}
			_ => {
				conn.sender.send(Message::Response(
					Response {
						id,
						result: Some(serde_json::Value::Null),
						error: None,
					}
				))?;

				return Ok(());
			}
		};

		let resp = Response {
			id,
			result: Some(serde_json::to_value(Hover {
				contents,
				range: None,
			})?),
			error: None,
		};

		conn.sender.send(Message::Response(resp))?;
		Ok(())
	}

	pub(super) fn zscript_req_semtokens_full(
		&self,
		conn: &Connection,
		gfile: GreenFile,
		id: RequestId,
	) -> UnitResult {
		let cursor = SyntaxNode::new_root(gfile.root);

		let resp = Response {
			id,
			result: Some(serde_json::to_value(SemanticTokensResult::Tokens(
				semtokens(cursor, gfile.lndx),
			))?),
			error: None,
		};

		conn.sender.send(Message::Response(resp))?;
		Ok(())
	}
}

pub(super) fn req_semtokens_range(
	conn: &Connection,
	gfile: GreenFile,
	id: RequestId,
	range: lsp_types::Range,
) -> UnitResult {
	let cursor = SyntaxNode::new_root(gfile.root);

	let lc_start = LineCol {
		line: range.start.line,
		col: range.end.character,
	};

	let lc_end = LineCol {
		line: range.end.line,
		col: range.end.character,
	};

	let Some(start) = gfile.lndx.offset(lc_start) else {
		return Err(Box::new(MsgError(format!("invalid range start: {lc_start:?}"))));
	};

	let Some(end) = gfile.lndx.offset(lc_end) else {
		return Err(Box::new(MsgError(format!("invalid range end: {lc_end:?}"))));
	};

	let node = match cursor.covering_element(TextRange::new(start, end)) {
		NodeOrToken::Node(node) => node,
		NodeOrToken::Token(token) => token.parent().unwrap(),
	};

	let resp = Response {
		id,
		result: Some(serde_json::to_value(SemanticTokensRangeResult::Tokens(
			semtokens(node, gfile.lndx),
		))?),
		error: None,
	};

	conn.sender.send(Message::Response(resp))?;
	Ok(())
}

#[must_use]
pub(self) fn semtokens(node: SyntaxNode, lndx: Arc<LineIndex>) -> SemanticTokens {
	let mut context = highlight::Context {
		hl: Highlighter::new(lndx),
	};

	highlight::traverse(&mut context, node);

	SemanticTokens {
		result_id: None,
		data: context.hl.tokens,
	}
}

impl Project {
	/// Returns:
	/// - `Continue` if the symbol hasn't been found.
	/// - `Break(Ok)` if the symbol was resolved successfully.
	fn zscript_req_goto_ident(
		&mut self,
		name: &str,
	) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
		let symtables = self
			.sources
			.iter()
			.copied()
			.filter_map(|fileid| self.symbol_table(fileid))
			.collect::<Vec<_>>();

		let symkey = symtables
			.par_iter()
			.find_map_any(|symtable| symtable.get(&QName::new_for_type(name)));

		match symkey {
			Some(project::SymbolKey::ZScript(sym_k)) => self.goto_symbol(*sym_k, name),
			None => ControlFlow::Continue(()),
		}
	}

	#[must_use]
	fn goto_symbol(
		&mut self,
		sym_k: SymbolKey,
		name: &str,
	) -> ControlFlow<Result<GotoDefinitionResponse, ErrorBox>> {
		let filepos = match sym_k {
			SymbolKey::Class(class_k) => self.lookup_zscript_class(class_k).filepos,
			SymbolKey::Constant(const_k) => self.lookup_zscript_constant(const_k).filepos,
			SymbolKey::Enum(enum_k) => self.lookup_zscript_enum(enum_k).filepos,
			SymbolKey::Mixin(mixin_k) => self.lookup_zscript_mixin_class(mixin_k).filepos,
			SymbolKey::Struct(struct_k) => self.lookup_zscript_struct(struct_k).filepos,
		};

		let path = self.get_path(filepos.file).unwrap();
		let gfile = self.lookup_gfile(self.get_gfile(filepos.file));

		match util::make_location(&gfile.lndx, path, filepos.pos, name.len()) {
			Ok(l) => ControlFlow::Break(Ok(GotoDefinitionResponse::Scalar(l))),
			Err(err) => ControlFlow::Break(Err(Box::new(err))),
		}
	}

	/// Uses the [`rayon`] global thread pool.
	/// In the `Err` case, the root itself could not be read.
	/// `root` will always have the file stem `ZSCRIPT` (any ASCII casing),
	/// with one or no extension.
	/// Elements in `zpaths` are absolute (from LSP `file` URIs).
	pub(crate) fn rebuild_zscript_include_tree(
		&mut self,
		zs_root: PathBuf,
	) -> std::io::Result<Vec<Diagnostic>> {
		#[derive(Debug, Clone, Copy)]
		struct Context<'p> {
			paths: &'p FxIndexSet<PathBuf>,
			zpaths: &'p FxIndexSet<ZPathBuf>,
			output: &'p Mutex<Vec<StagedFile>>,
			diags: &'p Mutex<Vec<Diagnostic>>,
		}

		#[derive(Debug)]
		struct StagedFile {
			path: PathBuf,
			source: project::Source,
			root: GreenNode,
		}

		fn recur(ctx: Context, base: &Path, path: PathBuf, text: Arc<str>) {
			let ptree = doomfront::parse(
				&text,
				doomfront::zdoom::zscript::parse::file,
				// TODO: Either user-configurable or from the root.
				doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
			);

			let root = ptree.into_inner();
			let lndx = LineIndex::new(&text);
			let incpaths = collect_include_paths(ctx, &lndx, root.clone());

			let source = project::Source {
				lang: LangId::ZScript,
				text,
				lndx: Arc::new(lndx),
			};

			incpaths
				.into_par_iter()
				.for_each(|(incpath, incpath_range)| {
					let complete = match base.join(&incpath).canonicalize() {
						Ok(c) => c,
						Err(err) => {
							tracing::error!(
								"Failed to canonicalize include path: `{}` - {err}",
								incpath.display()
							);
							return;
						}
					};

					let Some(file_id) = ctx
						.zpaths
						.get_index_of(ZPath::new(&complete))
						.map(|i| FileId(i as u32)) else {
							tracing::error!("Failed to get interned ZDoom path: `{}`", complete.display());
							return;
						};

					let real_path = ctx.paths.get_index(file_id.0 as usize).unwrap();

					let text = match std::fs::read_to_string(real_path) {
						Ok(s) => Arc::from(s),
						Err(err) => {
							ctx.diags.lock().push(Diagnostic {
								range: util::make_range(&source.lndx, incpath_range),
								severity: Some(DiagnosticSeverity::ERROR),
								code: None,
								code_description: None,
								source: None,
								message: format!(
									"Failed to read file: `{}` - {err}",
									real_path.display()
								),
								related_information: None,
								tags: None,
								data: None,
							});

							return;
						}
					};

					let base = complete.parent().unwrap();
					recur(ctx, base, real_path.to_path_buf(), text);
				});

			ctx.output.lock().push(StagedFile { path, source, root });
		}

		#[must_use]
		fn collect_include_paths(
			ctx: Context,
			lndx: &LineIndex,
			root: GreenNode,
		) -> Vec<(PathBuf, TextRange)> {
			let cursor = SyntaxNode::new_root(root);

			cursor
				.children()
				.filter_map(|child| {
					if child.kind() != Syn::IncludeDirective {
						return None;
					}

					// This is infallible, since an include directive must have
					// at least an `#include` token.
					let last_token = child.last_token().unwrap();

					if last_token.kind() != Syn::StringLit {
						ctx.diags.lock().push(Diagnostic {
							range: util::make_range(lndx, last_token.text_range()),
							severity: Some(DiagnosticSeverity::ERROR),
							code: None,
							code_description: None,
							source: None,
							message: format!("Expected a string, found: {:#?}", last_token.kind()),
							related_information: None,
							tags: None,
							data: None,
						});
						return None;
					};

					let text = last_token.text();

					if !text.is_empty() {
						Some((
							PathBuf::from(&text[1..(text.len() - 1)]),
							last_token.text_range(),
						))
					} else {
						ctx.diags.lock().push(Diagnostic {
							range: util::make_range(lndx, last_token.text_range()),
							severity: Some(DiagnosticSeverity::ERROR),
							code: None,
							code_description: None,
							source: None,
							message: "Expected a path, found an empty string".to_string(),
							related_information: None,
							tags: None,
							data: None,
						});
						None
					}
				})
				.collect()
		}

		let mut output = vec![];
		let text = std::fs::read_to_string(&zs_root)?;
		let lndx = LineIndex::new(&text);
		let ptree = doomfront::parse(
			&text,
			doomfront::zdoom::zscript::parse::file,
			// TODO: Either user-configurable or from this root.
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		);
		let text: Arc<str> = text.into();

		output.push(StagedFile {
			path: zs_root.clone(),
			source: project::Source {
				lang: LangId::ZScript,
				text: text.clone(),
				lndx: Arc::new(lndx),
			},
			root: ptree.into_inner(),
		});

		let diags = Mutex::new(vec![]);
		let output = Mutex::new(output);

		let ctx = Context {
			paths: &self.paths,
			zpaths: &self.zpaths,
			output: &output,
			diags: &diags,
		};

		recur(ctx, self.root(), zs_root, text);
		let output = output.into_inner();

		for file in output {
			let file_id = self.intern_pathbuf(file.path);
			let lndx = file.source.lndx.clone();
			self.set_source(file_id, file.source);
			self.sources.insert(file_id);

			self.gfile(GreenFile {
				lang: LangId::ZScript,
				root: file.root,
				lndx,
			});
		}

		Ok(diags.into_inner())
	}
}

// Frontend impls //////////////////////////////////////////////////////////////

#[must_use]
pub(crate) fn zscript_symbol_table(
	db: &dyn Frontend,
	file_id: FileId,
	gfile: GreenFile,
) -> SymbolTable {
	let mut ret = SymbolTable::default();
	let cursor = SyntaxNode::new_root(gfile.root);

	for child in cursor.children() {
		let Some(top) = ast::TopLevel::cast(child) else { continue; };

		match top {
			ast::TopLevel::ClassDef(classdef) => {
				if let Ok(class_name) = classdef.name() {
					let datum = ClassDatum {
						filepos: FilePos {
							file: file_id,
							pos: class_name.text_range().start(),
						},
					};

					let key = db.zscript_class(Arc::new(datum));

					ret.insert(
						QName::new_for_type(class_name.text()),
						project::SymbolKey::ZScript(SymbolKey::Class(key)),
					);
				}
			}
			ast::TopLevel::ConstDef(constdef) => {
				if let Ok(const_name) = constdef.name() {
					let datum = ConstantDatum {
						filepos: FilePos {
							file: file_id,
							pos: const_name.text_range().start(),
						},
					};

					let key = db.zscript_constant(Arc::new(datum));

					ret.insert(
						QName::new_for_type(const_name.text()),
						project::SymbolKey::ZScript(SymbolKey::Constant(key)),
					);
				}
			}
			ast::TopLevel::EnumDef(enumdef) => {
				if let Ok(enum_name) = enumdef.name() {
					let datum = EnumDatum {
						filepos: FilePos {
							file: file_id,
							pos: enum_name.text_range().start(),
						},
					};

					let key = db.zscript_enum(Arc::new(datum));

					ret.insert(
						QName::new_for_type(enum_name.text()),
						project::SymbolKey::ZScript(SymbolKey::Enum(key)),
					);
				}
			}
			ast::TopLevel::MixinClassDef(mixindef) => {
				if let Ok(mixin_name) = mixindef.name() {
					let datum = MixinClassDatum {
						filepos: FilePos {
							file: file_id,
							pos: mixin_name.text_range().start(),
						},
					};

					let key = db.zscript_mixin_class(Arc::new(datum));

					ret.insert(
						QName::new_for_type(mixin_name.text()),
						project::SymbolKey::ZScript(SymbolKey::Mixin(key)),
					);
				}
			}
			ast::TopLevel::StructDef(structdef) => {
				if let Ok(struct_name) = structdef.name() {
					let datum = StructDatum {
						filepos: FilePos {
							file: file_id,
							pos: struct_name.text_range().start(),
						},
					};

					let key = db.zscript_struct(Arc::new(datum));

					ret.insert(
						QName::new_for_type(struct_name.text()),
						project::SymbolKey::ZScript(SymbolKey::Struct(key)),
					);
				}
			}
			ast::TopLevel::ClassExtend(_)
			| ast::TopLevel::StructExtend(_)
			| ast::TopLevel::Include(_)
			| ast::TopLevel::Version(_) => continue,
		}
	}

	ret
}

/// `text` must be the file's entire content.
/// `green` must be tagged [`Syn::Root`]; the returned node is tagged as such too.
#[must_use]
pub(crate) fn _splicing_reparse(
	text: &str,
	green: GreenNode,
	changed: (TextRange, TextSize),
) -> GreenNode {
	use doomfront::zdoom::zscript::parse;

	let cursor = SyntaxNode::new_root(green.clone());

	let to_reparse = cursor
		.children()
		.find(|node| node.text_range().contains_range(changed.0));

	let Some(to_reparse) = to_reparse else {
		return doomfront::parse(
			text,
			parse::file,
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		).into_inner();
	};

	let parser = match to_reparse.kind() {
		Syn::Error => {
			return doomfront::parse(
				text,
				parse::file,
				doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
			)
			.into_inner();
		}
		Syn::ClassDef => parse::class_def,
		Syn::MixinClassDef => parse::mixin_class_def,
		Syn::StructDef => parse::struct_def,
		Syn::ClassExtend | Syn::StructExtend => parse::class_or_struct_extend,
		Syn::EnumDef => parse::enum_def,
		Syn::IncludeDirective => parse::include_directive,
		Syn::VersionDirective => parse::version_directive,
		Syn::ConstDef => parse::const_def,
		_ => unreachable!(),
	};

	let old_range = to_reparse.text_range();
	let new_end = old_range.end().min(changed.0.start() + changed.1);

	let new_child = doomfront::parse(
		&text[TextRange::new(old_range.start(), new_end + TextSize::from(1))],
		parser,
		doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
	)
	.into_inner();

	green.replace_child(to_reparse.index(), NodeOrToken::Node(new_child))
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn smoke_splicing_reparse() {
		const SOURCE: &str = r#"

/// This class undoubtedly does something remarkable.
class Something : SomethingElse {
	private uint a_field;
}

"#;

		let green = doomfront::parse(
			SOURCE,
			doomfront::zdoom::zscript::parse::file,
			doomfront::zdoom::lex::Context::ZSCRIPT_LATEST,
		)
		.into_inner();

		const SOURCE_CHANGED: &str = r#"

/// This class undoubtedly does something remarkable.
class Something : SomethingElse {}

"#;

		let mut source = SOURCE.to_string();
		source.replace_range(89..113, "");
		assert_eq!(source, SOURCE_CHANGED);

		let _ = _splicing_reparse(
			&source,
			green,
			(
				TextRange::new(TextSize::from(89), TextSize::from(113)),
				TextSize::from(0),
			),
		);
	}
}
