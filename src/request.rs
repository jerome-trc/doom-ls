use std::{fmt::Write, ops::ControlFlow};

use lsp_server::{
	Connection, ErrorCode, ExtractError, Request, RequestId, Response, ResponseError,
};
use lsp_types::{
	request::{
		DocumentSymbolRequest, HoverRequest, SemanticTokensFullRequest, SemanticTokensRangeRequest,
	},
	OneOf, TextDocumentIdentifier,
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
		let mut log = "\r\n".to_string();

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
					let _ = writeln!(log, "{:#?} refers to `{}`", crit_span, decl);
				}
				OneOf::Right(in_sym) => {
					let _ = writeln!(log, "{:#?} refers to `{}`", sgk.span, in_sym.decl);
				}
			}
		}

		// TODO: would be better if this got emitted as a virtual file.
		tracing::debug!("{log}");
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
