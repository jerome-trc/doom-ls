//! Routines acting on [`Core`] for handling client requests.

use std::ops::ControlFlow;

use lsp_server::{Connection, Request, RequestId};
use lsp_types::request::{
	GotoDefinition, HoverRequest, SemanticTokensFullRequest, SemanticTokensRangeRequest,
};

use crate::{project::Compiler, util, zscript, Core, LangId, UnitResult};

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

		let Some(gfile_k) = project.try_get_gfile(file_id) else {
			return Core::respond_null(conn, id);
		};

		let gfile = project.lookup_gfile(gfile_k);

		match gfile.lang {
			LangId::ZScript => {
				return zscript::req_semtokens_range(conn, gfile, id, params.range);
			}
			_ => Core::respond_null(conn, id),
		}
	})?;

	req = try_request::<HoverRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document_position_params.text_document.uri)?;

		let Some((project, file_id)) = core.find_project_by_path(&path) else {
			return Core::respond_null(conn, id);
		};

		let Some(gfile_k) = project.try_get_gfile(file_id) else {
			return Core::respond_null(conn, id);
		};

		let gfile = project.lookup_gfile(gfile_k);

		match gfile.lang {
			LangId::ZScript => {
				return core.zscript_req_hover(conn, gfile, id, params);
			}
			_ => Core::respond_null(conn, id),
		}
	})?;

	req = try_request::<SemanticTokensFullRequest, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;

		let Some((project, file_id)) = core.find_project_by_path(&path) else {
			return Core::respond_null(conn, id);
		};

		let Some(gfile_k) = project.try_get_gfile(file_id) else {
			return Core::respond_null(conn, id);
		};

		let gfile = project.lookup_gfile(gfile_k);

		match gfile.lang {
			LangId::ZScript => {
				return core.zscript_req_semtokens_full(conn, gfile, id);
			}
			_ => Core::respond_null(conn, id),
		}
	})?;

	req = try_request::<GotoDefinition, _>(req, |id, params| {
		let path = util::uri_to_pathbuf(&params.text_document_position_params.text_document.uri)?;

		let Some((ix_p, gfile)) = core.projects.iter().enumerate().find_map(|(i, project)| {
			let file_id = match project.get_fileid(&path) {
				Some(id) => id,
				None => {
					return None;
				},
			};

			project.try_get_gfile(file_id).map(|gf_k| {
				(i, project.lookup_gfile(gf_k))
			})
		}) else {
			return Core::respond_null(conn, id);
		};

		let LangId::ZScript = gfile.lang else { return Core::respond_null(conn, id); };

		core.zscript_req_goto(
			conn,
			gfile,
			id,
			params.text_document_position_params.position,
			ix_p,
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
