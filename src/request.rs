//! Routines acting on [`Core`] for handling requests from the client.

use std::ops::ControlFlow;

use lsp_server::{Connection, Request, RequestId};
use lsp_types::request::{
	GotoDefinition, HoverRequest, SemanticTokensFullRequest, SemanticTokensRangeRequest,
};

use crate::{util, zscript, Core, LangId, UnitResult};

pub(super) fn handle(
	core: &mut Core,
	conn: &Connection,
	mut req: Request,
) -> ControlFlow<UnitResult, Request> {
	req = try_request::<SemanticTokensRangeRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;

		let Some((project, file_id)) = core.find_project_by_path(&path) else {
			return Core::respond_null(conn, id);
		};

		let Some(sfile) = project.get_file(file_id) else {
			return Core::respond_null(conn, id);
		};

		match sfile.lang {
			LangId::ZScript => {
				return zscript::req_semtokens_range(conn, sfile, id, params.range);
			}
			_ => Core::respond_null(conn, id),
		}
	})?;

	req = try_request::<SemanticTokensFullRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;

		let Some((project, file_id)) = core.find_project_by_path(&path) else {
			return Core::respond_null(conn, id);
		};

		let Some(sfile) = project.get_file(file_id) else {
			return Core::respond_null(conn, id);
		};

		match sfile.lang {
			LangId::ZScript => {
				return zscript::req_semtokens_full(conn, sfile, id);
			}
			_ => Core::respond_null(conn, id),
		}
	})?;

	ControlFlow::Continue(req)
}

#[must_use]
fn try_request<R, F>(req: Request, callback: F) -> ControlFlow<UnitResult, Request>
where
	R: lsp_types::request::Request,
	F: FnOnce(RequestId, R::Params) -> UnitResult,
{
	match req.extract::<R::Params>(R::METHOD) {
		Ok((reqid, params)) => ControlFlow::Break(callback(reqid, params)),
		Err(err) => Core::extract_error(err),
	}
}
