//! Routines acting on [`Core`] for handling requests from the client.

use std::ops::ControlFlow;

use lsp_server::{Connection, Request, RequestId};
use lsp_types::request::{
	GotoDefinition, HoverRequest, References, SemanticTokensFullRequest, SemanticTokensRangeRequest,
};

use crate::{util, zscript, Core, LangId, UnitResult};

pub(super) fn handle(
	core: &mut Core,
	conn: &Connection,
	mut req: Request,
) -> ControlFlow<UnitResult, Request> {
	req = try_request::<SemanticTokensRangeRequest, _>(req, |id, params| {
		core.semantic_update();
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

	req = try_request::<HoverRequest, _>(req, |id, params| {
		core.semantic_update();
		let path = util::uri_to_pathbuf(&params.text_document_position_params.text_document.uri)?;

		let Some((project, file_id)) = core.find_project_by_path(&path) else {
			return Core::respond_null(conn, id);
		};

		let Some(sfile) = project.get_file(file_id) else {
			return Core::respond_null(conn, id);
		};

		match sfile.lang {
			LangId::ZScript => {
				return zscript::req_hover(conn, sfile, id, params);
			}
			_ => Core::respond_null(conn, id),
		}
	})?;

	req = try_request::<SemanticTokensFullRequest, _>(req, |id, params| {
		core.semantic_update();
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

	req = try_request::<GotoDefinition, _>(req, |id, params| {
		core.semantic_update();
		let path = util::uri_to_pathbuf(&params.text_document_position_params.text_document.uri)?;

		let Some((project, ix_p, file_id)) = core.projects.iter().enumerate().find_map(|(i, p)| {
			p.get_fileid(&path).map(|file_id| (p, i, file_id))
		}) else {
			tracing::debug!("GotoDefinition miss - file not in load order.");
			return Core::respond_null(conn, id);
		};

		let Some(sfile) = project.get_file(file_id) else {
			tracing::debug!("GotoDefinition miss - file not loaded.");
			return Core::respond_null(conn, id);
		};

		let LangId::ZScript = sfile.lang else {
			tracing::debug!("GotoDefinition miss - unsupported language.");
			return Core::respond_null(conn, id);
		};

		zscript::req_goto(
			core,
			conn,
			sfile,
			id,
			params.text_document_position_params.position,
			ix_p,
		)
	})?;

	req = try_request::<References, _>(req, |id, params| {
		core.semantic_update();
		let path = util::uri_to_pathbuf(&params.text_document_position.text_document.uri)?;

		let Some((project, ix_p, file_id)) = core.projects.iter().enumerate().find_map(|(i, p)| {
			p.get_fileid(&path).map(|file_id| (p, i, file_id))
		}) else {
			return Core::respond_null(conn, id);
		};

		let Some(sfile) = project.get_file(file_id) else {
			return Core::respond_null(conn, id);
		};

		let LangId::ZScript = sfile.lang else {
			return Core::respond_null(conn, id);
		};

		zscript::req_references(
			core,
			conn,
			id,
			params.text_document_position.position,
			ix_p,
			sfile,
		)
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
