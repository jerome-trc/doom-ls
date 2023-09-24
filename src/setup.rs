use std::path::Path;

use lsp_server::{Connection, Response};
use lsp_types::{
	request::WorkspaceConfiguration, HoverProviderCapability, OneOf, SaveOptions,
	ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
	TextDocumentSyncSaveOptions, MessageType,
};
use tracing::{debug, error};
use tracing_subscriber::{
	fmt::writer::BoxMakeWriter, prelude::__tracing_subscriber_SubscriberExt,
	util::SubscriberInitExt,
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
		/*

		TODO

		definition_provider: Some(OneOf::Left(true)),
		document_symbol_provider: Some(OneOf::Left(true)),
		hover_provider: Some(HoverProviderCapability::Simple(true)),
		references_provider: Some(OneOf::Left(true)),
		semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
			SemanticTokensOptions {
				work_done_progress_options: WorkDoneProgressOptions::default(),
				legend: crate::semtok::legend(),
				range: Some(true),
				full: Some(SemanticTokensFullOptions::Bool(true)),
			},
		)),

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
