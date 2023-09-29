//! Functions for work that has to be done before [`crate::main_loop`] starts.

use std::{
	path::{Path, PathBuf},
	time::Instant,
};

use lsp_server::{Connection, Message, Notification, Response};
use lsp_types::{
	notification::PublishDiagnostics, request::WorkspaceConfiguration, HoverProviderCapability,
	MessageType, OneOf, PublishDiagnosticsParams, SaveOptions, SemanticTokensFullOptions,
	SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities,
	TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
	TextDocumentSyncSaveOptions, Url, WorkDoneProgressOptions,
};
use tracing::{debug, error};
use tracing_subscriber::{
	fmt::writer::BoxMakeWriter, prelude::__tracing_subscriber_SubscriberExt,
	util::SubscriberInitExt,
};

use crate::{
	core::{Core, Project, Source},
	error::Error,
	langs::{self, LangId},
	lines::LineIndex,
	util, FxHamt,
};

pub(crate) fn logging() {
	/// Like [`tracing_subscriber::fmt::time::Uptime`] but with
	/// hour/minute/second formatting for better clarity.
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	struct Uptime(std::time::Instant);

	impl Default for Uptime {
		fn default() -> Self {
			Self(std::time::Instant::now())
		}
	}

	impl tracing_subscriber::fmt::time::FormatTime for Uptime {
		fn format_time(
			&self,
			w: &mut tracing_subscriber::fmt::format::Writer<'_>,
		) -> std::fmt::Result {
			let elapsed = self.0.elapsed();
			let mins = elapsed.as_secs() / 60;
			let hours = mins / 60;
			let secs = elapsed.as_secs() % 60;
			write!(w, "{hours:02}:{mins:02}:{secs:02}")
		}
	}

	let timer = Uptime::default();
	eprintln!("Attempting log initialization.");
	let layer_stdout = tracing_subscriber::fmt::Layer::default()
		.with_timer(timer)
		.with_ansi(false)
		.with_writer(BoxMakeWriter::new(std::io::stderr));
	let collector = tracing_subscriber::registry().with(layer_stdout);
	collector.init();
}

#[must_use]
pub(crate) fn capabilities() -> ServerCapabilities {
	ServerCapabilities {
		document_symbol_provider: Some(OneOf::Left(true)),
		hover_provider: Some(HoverProviderCapability::Simple(true)),
		semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
			SemanticTokensOptions {
				work_done_progress_options: WorkDoneProgressOptions::default(),
				legend: crate::semtok::legend(),
				range: Some(true),
				full: Some(SemanticTokensFullOptions::Bool(true)),
			},
		)),
		/*

		TODO

		definition_provider: Some(OneOf::Left(true)),
		references_provider: Some(OneOf::Left(true)),

		*/
		text_document_sync: Some(TextDocumentSyncCapability::Options(
			TextDocumentSyncOptions {
				open_close: Some(true),
				change: Some(TextDocumentSyncKind::INCREMENTAL),
				will_save: None,
				will_save_wait_until: None,
				save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
					include_text: None,
				})),
			},
		)),
		..Default::default()
	}
}

pub(crate) fn core(conn: &Connection, response: Response) -> Result<Core, Error> {
	let mut core = Core::default();

	let Some(resp) = response.result else {
		return Ok(core);
	};

	type CfgReqResult = <WorkspaceConfiguration as lsp_types::request::Request>::Result;

	let configs = match serde_json::from_value::<CfgReqResult>(resp) {
		Ok(c) => c,
		Err(err) => {
			return Err(Error::Process {
				source: Some(Box::new(err)),
				ctx: "failed to decode user config".to_string(),
			});
		}
	};

	let mut lo_opt = None;

	for cval in configs.iter() {
		let Some(obj) = cval.as_object() else {
			continue;
		};

		if let Some(v) = obj.get("loadOrder") {
			lo_opt = v.as_array()
		}
	}

	let Some(load_order) = lo_opt else {
		util::message(
			conn,
			"No load order is configured; DoomLS functionality will be minimal.".to_string(),
			MessageType::INFO,
		)?;

		return Ok(core);
	};

	for value in load_order {
		let Some(string) = value.as_str() else {
			error!("Non-string load order item given: {value:#?}");
			continue;
		};

		let project_path = Path::new(string);

		if !project_path.exists() {
			util::message(
				conn,
				format!("Load order item does not exist: {}", project_path.display()),
				MessageType::INFO,
			)?;

			continue;
		}

		debug!("Registering project: {}", project_path.display());

		let mut project = Project {
			root: project_path.to_owned(),
			files: FxHamt::default(),
			zscript: langs::zscript::IncludeTree::default(),
		};

		let read_dir = std::fs::read_dir(project_path)?;

		for result in read_dir {
			let Ok(rde) = result else {
				util::message(
					conn,
					format!("Failed to inspect file: {}", project_path.display()),
					MessageType::WARNING,
				)?;

				continue;
			};

			let path = rde.path();

			if path.is_symlink() {
				continue;
			} else if path.is_dir() {
				walk_dir(&mut core, conn, &mut project, path)?;
				continue;
			}

			let Some(fstem) = path.file_stem() else {
				continue;
			};

			let text = match std::fs::read_to_string(&path) {
				Ok(t) => t,
				Err(err) => {
					if err.kind() != std::io::ErrorKind::InvalidData {
						util::message(
							conn,
							format!("Failed to read file in project: {}", path.display()),
							MessageType::WARNING,
						)?;
					}

					continue;
				}
			};

			let file_id = core.paths.intern(&path);
			let lines = LineIndex::new(&text);

			project.files.insert(
				file_id,
				Source {
					id: file_id,
					lang: LangId::Unknown,
					text,
					lines,
					green: None,
				},
			);

			if fstem.eq_ignore_ascii_case("zscript") {
				project.zscript.root = Some(file_id);
			}
		}

		core.pending.projects.push(project);
	}

	let start_time = Instant::now();
	langs::zscript::inctree::walk(&mut core);

	debug!(
		"Include trees walked in {}ms.",
		start_time.elapsed().as_millis()
	);

	for (file_id, diags) in core.pending.malformed.iter() {
		let p = core.paths.resolve(*file_id);

		conn.sender.send(Message::Notification(Notification {
			method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
				.to_string(),
			params: serde_json::to_value(PublishDiagnosticsParams {
				uri: Url::from_file_path(p).unwrap(),
				diagnostics: diags.clone(),
				version: None,
			})
			.unwrap(),
		}))?;
	}

	Ok(core)
}

fn walk_dir(
	core: &mut Core,
	conn: &Connection,
	project: &mut Project,
	path: PathBuf,
) -> Result<(), Error> {
	let read_dir = std::fs::read_dir(&path)?;

	for result in read_dir {
		let Ok(rde) = result else {
			util::message(
				conn,
				format!("Failed to inspect file: {}", path.display()),
				MessageType::WARNING,
			)?;

			continue;
		};

		let path = rde.path();

		if path.is_symlink() {
			continue;
		} else if path.is_dir() {
			walk_dir(core, conn, project, path)?;
			continue;
		}

		let text = match std::fs::read_to_string(&path) {
			Ok(t) => t,
			Err(err) => {
				if err.kind() != std::io::ErrorKind::InvalidData {
					util::message(
						conn,
						format!("Failed to read file in project: {}", path.display()),
						MessageType::WARNING,
					)?;
				}

				continue;
			}
		};

		let file_id = core.paths.intern(&path);
		let lines = LineIndex::new(&text);

		project.files.insert(
			file_id,
			Source {
				id: file_id,
				lang: LangId::Unknown,
				text,
				lines,
				green: None,
			},
		);
	}

	Ok(())
}
