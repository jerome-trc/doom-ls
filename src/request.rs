use std::{fmt::Write, ops::ControlFlow};

use doomfront::{rowan::WalkEvent, LangExt};
use lsp_server::{
	Connection, ErrorCode, ExtractError, Request, RequestId, Response, ResponseError,
};
use lsp_types::{
	request::{
		DocumentSymbolRequest, HoverRequest, SemanticTokensFullRequest, SemanticTokensRangeRequest,
	},
	MessageType, OneOf, TextDocumentIdentifier,
};
use serde::{Deserialize, Serialize};
use tracing::debug;

use crate::{
	core::{Project, Source, SymGraphKey, SymGraphValue},
	langs::{self, LangId},
	util, Core, Error, UnitResult,
};

pub(crate) struct Context<'c> {
	pub(crate) core: &'c Core,
	pub(crate) conn: &'c Connection,
	pub(crate) id: RequestId,
	#[allow(unused)]
	pub(crate) project: &'c Project,
	pub(crate) src: &'c Source,
}

pub(super) fn handle(
	core: &mut Core,
	conn: &Connection,
	mut req: Request,
) -> ControlFlow<UnitResult, Request> {
	req = try_request::<SemanticTokensRangeRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;
		let file_id = core.paths.intern(&path);

		let Some((_, project)) = core.project_with(file_id) else {
			return util::respond_null(conn, id);
		};

		let src = project.files.get(&file_id).unwrap();

		let ctx = Context {
			core,
			conn,
			project,
			id,
			src,
		};

		match src.lang {
			LangId::ZScript => langs::zscript::semtok::range(ctx, params.range),
			LangId::CVarInfo | LangId::Decorate => util::respond_null(conn, ctx.id), // TODO
			LangId::Unknown => util::respond_null(conn, ctx.id),
		}
	})?;

	req = try_request::<HoverRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document_position_params.text_document.uri)?;
		let file_id = core.paths.intern(&path);

		let Some((_, project)) = core.project_with(file_id) else {
			#[cfg(debug_assertions)]
			debug!(
				"Hover info miss: no project contains file {:#?} ({})",
				file_id,
				path.display()
			);

			return util::respond_null(conn, id);
		};

		let src = project.files.get(&file_id).unwrap();

		let ctx = Context {
			core,
			conn,
			id,
			project,
			src,
		};

		match src.lang {
			LangId::Unknown => {
				#[cfg(debug_assertions)]
				debug!("Hover info miss: file has unknown language");

				util::respond_null(conn, ctx.id)
			}
			LangId::CVarInfo | LangId::Decorate => util::respond_null(conn, ctx.id), // TODO
			LangId::ZScript => langs::zscript::hover::handle(ctx, params),
		}
	})?;

	req = try_request::<SemanticTokensFullRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;
		let file_id = core.paths.intern(&path);

		let Some((_, project)) = core.project_with(file_id) else {
			return util::respond_null(conn, id);
		};

		let src = project.files.get(&file_id).unwrap();

		let ctx = Context {
			core,
			conn,
			project,
			id,
			src,
		};

		match src.lang {
			LangId::ZScript => langs::zscript::semtok::full(ctx),
			LangId::CVarInfo | LangId::Decorate => util::respond_null(conn, ctx.id), // TODO
			LangId::Unknown => util::respond_null(conn, ctx.id),
		}
	})?;

	req = try_request::<DocumentSymbolRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;
		let file_id = core.paths.intern_owned(path);

		let Some((_, project)) = core.project_with(file_id) else {
			return util::respond_null(conn, id);
		};

		let src = project.files.get(&file_id).unwrap();

		if src.green.is_none() {
			return util::respond_null(conn, id);
		}

		match src.lang {
			LangId::Unknown => util::respond_null(conn, id),
			LangId::CVarInfo | LangId::Decorate => util::respond_null(conn, id), // TODO
			LangId::ZScript => langs::zscript::docsym::handle(conn, src, id),
		}
	})?;

	req = try_request::<DebugSymGraphRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;
		let file_id = core.paths.intern_owned(path);

		let Some((_, project)) = core.project_with(file_id) else {
			return util::respond_null(conn, id);
		};

		let src = project.files.get(&file_id).unwrap();

		if src.green.is_none() {
			return util::respond_null(conn, id);
		}

		tracing::debug!("References in current file's symbol graph:");
		let mut output = "\r\n".to_string();

		for (key, val) in core.ready.sym_graph.iter() {
			let SymGraphKey::Reference(sgk) = key else {
				continue;
			};

			if sgk.file_id != file_id {
				continue;
			}

			let SymGraphValue::Symbol(sym_ix) = val else {
				unreachable!()
			};

			match core.ready.symbol(*sym_ix) {
				OneOf::Left(u_sym) => {
					let crit_span = u_sym.crit_span;
					let decl = &src.text[crit_span];
					writeln!(output, "{:#?} refers to `{}`", crit_span, decl).unwrap();
				}
				OneOf::Right(in_sym) => {
					writeln!(output, "{:#?} refers to `{}`", sgk.span, in_sym.decl).unwrap();
				}
			}
		}

		// TODO: would be better if this got emitted as a virtual file.
		debug!("{output}");
		util::respond_null(conn, id)
	})?;

	req = try_request::<DebugTextRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;
		let file_id = core.paths.intern(&path);

		let Some((_, project)) = core.project_with(file_id) else {
			return util::respond_null(conn, id);
		};

		let src = project.files.get(&file_id).unwrap();

		debug!(
			"Server-side text of file {}:\r\n{}",
			path.display(),
			&src.text
		);

		util::respond_null(conn, id)
	})?;

	req = try_request::<DebugAstRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;
		let file_id = core.paths.intern_owned(path);

		let Some((_, project)) = core.project_with(file_id) else {
			return util::respond_null(conn, id);
		};

		let src = project.files.get(&file_id).unwrap();

		let Some(green) = src.green.as_ref().cloned() else {
			return util::respond_null(conn, id);
		};

		#[must_use]
		fn walk<L: LangExt>(cursor: doomfront::rowan::SyntaxNode<L>) -> String {
			let mut output = "\r\n".to_string();
			let mut depth = 0;

			for event in cursor.preorder_with_tokens() {
				match event {
					WalkEvent::Enter(elem) => {
						for _ in 0..depth {
							output.push_str("    ");
						}

						writeln!(output, "{elem:?}").unwrap();
						depth += 1;
					}
					WalkEvent::Leave(_) => {
						depth -= 1;
					}
				}
			}

			output
		}

		let output = match src.lang {
			LangId::Unknown => {
				util::message(
					conn,
					"This file's language is unsupported by DoomLS; no AST can be shown."
						.to_string(),
					MessageType::INFO,
				)?;

				return util::respond_null(conn, id);
			}
			LangId::CVarInfo => walk(doomfront::zdoom::cvarinfo::SyntaxNode::new_root(green)),
			LangId::Decorate => walk(doomfront::zdoom::decorate::SyntaxNode::new_root(green)),
			LangId::ZScript => walk(doomfront::zdoom::zscript::SyntaxNode::new_root(green)),
		};

		// TODO: would be better if this got emitted as a virtual file.
		debug!("{output}");
		util::respond_null(conn, id)
	})?;

	ControlFlow::Continue(req)
}

#[must_use]
fn try_request<R, F>(req: Request, callback: F) -> ControlFlow<UnitResult, Request>
where
	R: lsp_types::request::Request,
	F: FnOnce(RequestId, R::Params) -> UnitResult,
{
	let id = req.id.clone();

	match req.extract::<R::Params>(R::METHOD) {
		Ok((reqid, params)) => ControlFlow::Break(callback(reqid, params)),
		Err(err) => match err {
			ExtractError::MethodMismatch(t) => ControlFlow::Continue(t),
			ExtractError::JsonError { method: _, error } => {
				ControlFlow::Break(Err(Error::Response(Response {
					id,
					result: None,
					error: Some(ResponseError {
						code: ErrorCode::InvalidParams as i32,
						message: error.to_string(),
						data: None,
					}),
				})))
			}
		},
	}
}

#[derive(Debug)]
enum DebugTextRequest {}

impl lsp_types::request::Request for DebugTextRequest {
	type Params = DebugTextParams;
	type Result = ();

	const METHOD: &'static str = "doomls/debugText";
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct DebugTextParams {
	text_document: TextDocumentIdentifier,
}

#[derive(Debug)]
enum DebugAstRequest {}

impl lsp_types::request::Request for DebugAstRequest {
	type Params = DebugAstParams;
	type Result = ();

	const METHOD: &'static str = "doomls/debugAst";
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct DebugAstParams {
	text_document: TextDocumentIdentifier,
}

#[derive(Debug)]
enum DebugSymGraphRequest {}

impl lsp_types::request::Request for DebugSymGraphRequest {
	type Params = DebugSymGraphParams;
	type Result = ();

	const METHOD: &'static str = "doomls/symGraph";
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct DebugSymGraphParams {
	text_document: TextDocumentIdentifier,
}
