//! Routines acting on [`Core`] for handling notifications from the client.

use std::ops::ControlFlow;

use lsp_server::{Connection, Message, Notification};
use lsp_types::{
	notification::{
		DidChangeConfiguration, DidChangeTextDocument, DidChangeWatchedFiles, DidOpenTextDocument,
		DidSaveTextDocument, PublishDiagnostics,
	},
	FileChangeType, PublishDiagnosticsParams,
};
use tracing::debug;

use crate::{
	lines::{self, LineIndex},
	project::SourceFile,
	util, zscript, Core, LangId, UnitResult,
};

pub(super) fn handle(
	core: &mut Core,
	conn: &Connection,
	mut notif: Notification,
) -> ControlFlow<UnitResult, Notification> {
	notif = try_notif::<DidChangeTextDocument, _>(notif, |params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;

		let Some((project, file_id)) = core.find_project_by_path_mut(&path) else {
			// The user is editing a file outside the load order.
			// Nothing to do here.
			return Ok(());
		};

		let Some(sfile) = project.get_file_mut(file_id) else {
			// The edited file falls under the purview of the load order,
			// but it is of an unsupported language. Nothing to do here.
			return Ok(());
		};

		let deltas = lines::splice_changes(&mut sfile.text, params.content_changes);
		// TODO: Try reducing how many times the line index needs to be recomputed.
		sfile.lndx = LineIndex::new(&sfile.text);

		let result = if let Some(_deltas) = deltas {
			match sfile.lang {
				LangId::Unknown => Ok(()),
				LangId::ZScript => zscript::full_reparse(sfile),
			}
		} else {
			match sfile.lang {
				LangId::Unknown => Ok(()),
				LangId::ZScript => zscript::full_reparse(sfile),
			}
		};

		let diags = sfile.parse_diagnostics();

		conn.sender.send(Message::Notification(Notification {
			method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
				.to_string(),
			params: serde_json::to_value(PublishDiagnosticsParams {
				uri: params.text_document.uri,
				diagnostics: diags,
				version: None,
			})
			.unwrap(),
		}))?;

		project.set_dirty(file_id);
		result
	})?;

	notif = try_notif::<DidChangeWatchedFiles, _>(notif, |params| {
		for change in params.changes {
			let path = util::uri_to_pathbuf(&change.uri)?;

			match change.typ {
				FileChangeType::CHANGED => {
					let Some((project, file_id)) = core.find_project_by_path_mut(&path) else {
						continue;
					};

					let Some(sfile) = project.get_file_mut(file_id) else {
						continue;
					};

					let text = std::fs::read_to_string(&path)?;
					sfile.lndx = LineIndex::new(&text);
					sfile.text = text;
					project.set_dirty(file_id);
				}
				FileChangeType::CREATED => {
					let Some(project) = core.find_project_by_child_mut(&path) else {
						continue;
					};

					let text = std::fs::read_to_string(&path)?;
					let file_id = project.intern_pathbuf(path);
					let lndx = LineIndex::new(&text);

					let _ = project.set_file(
						file_id,
						SourceFile {
							lang: LangId::Unknown,
							text,
							lndx,
							parsed: None,
						},
					);
				}
				FileChangeType::DELETED => {
					let Some(project) = core.find_project_by_child_mut(&path) else {
						continue;
					};

					project.on_file_delete(path);
				}
				_ => unreachable!(),
			}
		}

		Ok(())
	})?;

	notif = try_notif::<DidChangeConfiguration, _>(notif, |params| {
		debug!("Config changed: {:#?}", params.settings);
		let Some(obj) = params.settings.as_object() else { return Ok(()); };
		let Some(v) = obj.get("loadOrder") else { return Ok(()); };
		let Some(string_array) = v.as_array() else { return Ok(()); };
		core.build_project_list(string_array);
		Ok(())
	})?;

	notif = try_notif::<DidSaveTextDocument, _>(notif, |params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;

		if path
			.file_stem()
			.is_some_and(|fstem| fstem.eq_ignore_ascii_case("ZSCRIPT"))
		{
			let Some(project) = core.find_project_by_child_mut(&path) else {
				return Ok(());
			};

			let _ = zscript::rebuild_include_tree(project, path);
			// TODO: Handle error, emit diagnostics.
		}

		Ok(())
	})?;

	notif = try_notif::<DidOpenTextDocument, _>(notif, |params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;

		let Some((project, file_id)) = core.find_project_by_path_mut(&path) else {
			// The user opened a file outside the load order. Nothing to do here.
			return Ok(());
		};

		let Some(sfile) = project.get_file_mut(file_id) else {
			// The opened file falls under the purview of the load order,
			// but it is of an unsupported language. Nothing to do here.
			return Ok(());
		};

		let diags = sfile.parse_diagnostics();

		conn.sender.send(Message::Notification(Notification {
			method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
				.to_string(),
			params: serde_json::to_value(PublishDiagnosticsParams {
				uri: params.text_document.uri,
				diagnostics: diags,
				version: None,
			})
			.unwrap(),
		}))?;

		Ok(())
	})?;

	ControlFlow::Continue(notif)
}

#[must_use]
fn try_notif<N, F>(notif: Notification, callback: F) -> ControlFlow<UnitResult, Notification>
where
	N: lsp_types::notification::Notification,
	F: FnOnce(N::Params) -> UnitResult,
{
	match notif.extract::<N::Params>(N::METHOD) {
		Ok(params) => ControlFlow::Break(callback(params)),
		Err(err) => Core::extract_error(err),
	}
}
