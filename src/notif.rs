//! Routines acting on [`Core`] for handling client notifications.

use std::{ops::ControlFlow, sync::Arc};

use lsp_server::Notification;
use lsp_types::{
	notification::{
		DidChangeConfiguration, DidChangeTextDocument, DidChangeWatchedFiles, DidSaveTextDocument,
	},
	FileChangeType,
};
use tracing::debug;

use crate::{
	lines::{self, LineIndex},
	project::{Compiler, Source},
	util, Core, LangId, UnitResult,
};

pub(super) fn handle(
	core: &mut Core,
	mut notif: Notification,
) -> ControlFlow<UnitResult, Notification> {
	notif = try_notif::<DidChangeTextDocument, _>(notif, |params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;

		let Some((project, file_id)) = core.find_project_by_path_mut(&path) else {
			// The user is editing a file outside the load order.
			// Nothing to do here.
			return Ok(());
		};

		let lang = match project.try_get_gfile(file_id) {
			Some(k) => project.lookup_gfile(k).lang,
			None => LangId::Unknown,
		};

		let mut new_text = project.source(file_id).text.to_string();

		lines::splice_changes(&mut new_text, params.content_changes);
		let lndx = Arc::new(LineIndex::new(&new_text));

		// TODO:
		// - Prevent needing to re-parse the entire file.
		// - Try reducing how many times the line index needs to be recomputed.

		project.set_source(
			file_id,
			Source {
				text: new_text.into(),
				lang,
				lndx,
			},
		);

		Ok(())
	})?;

	notif = try_notif::<DidChangeWatchedFiles, _>(notif, |params| {
		for change in params.changes {
			let path = util::uri_to_pathbuf(&change.uri)?;

			match change.typ {
				FileChangeType::CHANGED => {
					let Some((project, file_id)) = core.find_project_by_path_mut(&path) else {
						continue;
					};

					let Some(gfile_k) = project.try_get_gfile(file_id) else {
						continue;
					};

					let gfile = project.lookup_gfile(gfile_k);
					let text = std::fs::read_to_string(&path)?;
					let lndx = Arc::new(LineIndex::new(&text));

					project.set_source(
						file_id,
						Source {
							text: text.into(),
							lang: gfile.lang,
							lndx,
						},
					);
				}
				FileChangeType::CREATED => {
					let Some(project) = core.find_project_by_child_mut(&path) else {
						continue;
					};

					let text = std::fs::read_to_string(&path)?;
					let lndx = Arc::new(LineIndex::new(&text));
					let file_id = project.intern_pathbuf(path);

					project.set_source(
						file_id,
						Source {
							text: text.into(),
							lang: LangId::Unknown,
							lndx,
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

			let _ = project.rebuild_zscript_include_tree(path);
			// TODO: Handle error, emit diagnostics.
		}

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
