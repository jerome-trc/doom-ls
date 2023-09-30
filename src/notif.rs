use std::ops::ControlFlow;

use lsp_server::{Connection, ExtractError, Message, Notification};
use lsp_types::{
	notification::{
		DidChangeConfiguration, DidChangeTextDocument, DidCreateFiles, DidDeleteFiles,
		DidOpenTextDocument, DidRenameFiles, DidSaveTextDocument, PublishDiagnostics,
	},
	Diagnostic, DiagnosticSeverity, Position, PublishDiagnosticsParams, Url,
};

use crate::{core::Source, langs::LangId, lines::LineIndex, util, Core, Error, UnitResult};

pub(super) fn handle(
	core: &mut Core,
	conn: &Connection,
	mut notif: Notification,
) -> ControlFlow<UnitResult, Notification> {
	notif = try_notif::<DidChangeTextDocument, _>(notif, |params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;
		let file_id = core.paths.intern_owned(path);

		if core.project_with(file_id).is_none() {
			// The user is editing a file outside the load order.
			// Nothing to do here.
			return Ok(());
		};

		core.pending.dirty.insert(file_id, params);

		Ok(())
	})?;

	notif = try_notif::<DidChangeConfiguration, _>(notif, |params| {
		#[cfg(debug_assertions)]
		tracing::debug!("Config changed: {:#?}", params.settings);
		// TODO
		Ok(())
	})?;

	notif = try_notif::<DidSaveTextDocument, _>(notif, |_| Ok(()))?;

	notif = try_notif::<DidOpenTextDocument, _>(notif, |params| {
		let path = util::uri_to_pathbuf(&params.text_document.uri)?;
		let file_id = core.paths.intern(&path);

		let Some((_, project)) = core.project_with_mut(file_id) else {
			// The user opened a file entirely outside the load order.
			// Nothing to do here.
			return Ok(());
		};

		let src = project.files.get_mut(&file_id).unwrap();

		if src.lang == LangId::Unknown {
			src.lang = match params.text_document.language_id.as_str() {
				"cvarinfo" => LangId::CVarInfo,
				"decorate" => LangId::Decorate,
				"zscript" => LangId::ZScript,
				_ => LangId::Unknown,
			};
		}

		if src.lang == LangId::ZScript && !project.zscript.includes.contains_key(&file_id) {
			conn.sender
				.send(Message::Notification(unincluded_diag_notif(
					params.text_document.uri.clone(),
					"ZScript",
				)))?;
		}

		if let Some(diags) = core.pending.malformed.get(&file_id) {
			conn.sender.send(Message::Notification(Notification {
				method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
					.to_string(),
				params: serde_json::to_value(PublishDiagnosticsParams {
					uri: params.text_document.uri,
					diagnostics: diags.clone(),
					version: None,
				})
				.unwrap(),
			}))?;
		}

		Ok(())
	})?;

	notif = try_notif::<DidRenameFiles, _>(notif, |params| {
		for renamed in params.files {
			let old_path = util::parse_uri(&renamed.old_uri)?;
			let new_path = util::parse_uri(&renamed.new_uri)?;
			// TODO
			let _ = core.paths.intern(&old_path);
			let _ = core.paths.intern(&new_path);
		}

		Ok(())
	})?;

	notif = try_notif::<DidCreateFiles, _>(notif, |params| {
		for created in params.files {
			let path = util::parse_uri(&created.uri)?;
			let file_id = core.paths.intern(&path);

			let Some((i, _)) = core.project_by_child(&path) else {
				continue;
			};

			let text = std::fs::read_to_string(&path).map_err(Error::from)?;
			let lines = LineIndex::new(&text);

			let mut src = Source {
				id: file_id,
				lang: LangId::Unknown,
				text,
				green: None,
				lines,
			};

			let src = if core.pending.projects[i]
				.zscript
				.includes
				.contains_key(&file_id)
			{
				src.lang = LangId::ZScript;
				core.add_file(i, src)
			} else {
				src
			};

			core.pending.projects[i].files.insert(file_id, src.clone());
		}

		Ok(())
	})?;

	notif = try_notif::<DidDeleteFiles, _>(notif, |params| {
		for deletion in params.files {
			let path = util::parse_uri(&deletion.uri)?;
			let file_id = core.paths.intern_owned(path);
			core.on_file_delete(file_id);
		}

		Ok(())
	})?;

	ControlFlow::Continue(notif)
}

#[must_use]
fn unincluded_diag_notif(uri: Url, lang_name: &'static str) -> Notification {
	let diag = Diagnostic {
		range: lsp_types::Range {
			start: Position {
				line: 0,
				character: 0,
			},
			end: Position {
				line: 0,
				character: 0,
			},
		},
		severity: Some(DiagnosticSeverity::HINT),
		code: None,
		code_description: None,
		source: Some("doomls".to_string()),
		message: format!("file not part of {lang_name} include tree - DoomLS will do nothing"),
		related_information: None,
		tags: None,
		data: None,
	};

	Notification {
		method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string(),
		params: serde_json::to_value(PublishDiagnosticsParams {
			uri,
			diagnostics: vec![diag],
			version: None,
		})
		.unwrap(),
	}
}

#[must_use]
fn try_notif<N, F>(notif: Notification, callback: F) -> ControlFlow<UnitResult, Notification>
where
	N: lsp_types::notification::Notification,
	F: FnOnce(N::Params) -> UnitResult,
{
	match notif.extract::<N::Params>(N::METHOD) {
		Ok(params) => ControlFlow::Break(callback(params)),
		Err(err) => match err {
			ExtractError::MethodMismatch(t) => ControlFlow::Continue(t),
			ExtractError::JsonError { method: _, error } => {
				ControlFlow::Break(Err(Error::Process {
					ctx: format!("`{}` notification", N::METHOD),
					source: Some(Box::new(error)),
				}))
			}
		},
	}
}
