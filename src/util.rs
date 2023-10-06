//! Helper symbols that don't belong anywhere else.

use std::path::{Path, PathBuf};

use doomfront::{
	rowan::{SyntaxNode, SyntaxText},
	LangExt,
};
use lsp_server::{Connection, Message, Notification, RequestId, Response, ResponseError};
use lsp_types::{
	notification::{Notification as LspNotification, PublishDiagnostics, ShowMessage},
	Diagnostic, DiagnosticRelatedInformation, DiagnosticTag, MessageType, PublishDiagnosticsParams,
	ShowMessageParams, Url,
};
use tracing::error;

use crate::{error::Error, UnitResult};

/// `whitespace` is used to ensure that whitespace tokens have their contents
/// replaced with a single space character.
pub(crate) fn append_descendant_tokens<L: LangExt>(
	string: &mut String,
	node: &SyntaxNode<L>,
	whitespace: L::Kind,
) {
	for desc in node.descendants_with_tokens() {
		let Some(token) = desc.into_token() else {
			continue;
		};

		if token.kind() == whitespace {
			string.push(' ');
		} else {
			string.push_str(token.text());
		}
	}
}

/// Convenience function wrapping [`append_descendant_tokens`].
#[must_use]
pub(crate) fn descendant_tokens_to_string<L: LangExt>(
	node: &SyntaxNode<L>,
	whitespace: L::Kind,
) -> String {
	let mut ret = String::new();
	append_descendant_tokens(&mut ret, node, whitespace);
	ret
}

#[allow(unused)]
pub(crate) fn append_syntaxtext(string: &mut String, text: SyntaxText) {
	text.for_each_chunk(|chunk| {
		string.push_str(chunk);
	})
}

pub(crate) fn clear_diags(conn: &Connection, uri: Url) {
	send_diags(conn, uri, vec![]);
}

pub(crate) fn parse_uri(string: &str) -> Result<PathBuf, Error> {
	let uri = Url::parse(string).map_err(|err| Error::Process {
		source: Some(Box::new(err)),
		ctx: "parsing URI".to_string(),
	})?;

	uri_to_pathbuf(&uri)
}

pub(crate) fn message(conn: &Connection, text: String, msg_type: MessageType) -> UnitResult {
	conn.sender
		.send(Message::Notification(Notification {
			method: ShowMessage::METHOD.to_string(),
			params: serde_json::to_value(ShowMessageParams {
				typ: msg_type,
				message: text,
			})
			.unwrap(),
		}))
		.map_err(Error::from)
}

pub(crate) fn respond_null(conn: &Connection, id: RequestId) -> UnitResult {
	conn.sender
		.send(Message::Response(Response {
			id,
			result: Some(serde_json::Value::Null),
			error: None,
		}))
		.map_err(Error::from)
}

pub(crate) fn respond_err(conn: &Connection, id: RequestId, err: ResponseError) -> UnitResult {
	conn.sender
		.send(Message::Response(Response {
			id,
			result: None,
			error: Some(err),
		}))
		.map_err(Error::from)
}

pub(crate) fn root_dir_reader<'c, 'p: 'c>(
	conn: &'c Connection,
	path: &'p Path,
) -> Result<impl Iterator<Item = PathBuf> + 'c, Error> {
	let dir_reader = match std::fs::read_dir(path) {
		Ok(d_r) => d_r,
		Err(err) => {
			return Err(Error::Process {
				source: Some(Box::new(err)),
				ctx: "dir. reader creation".to_string(),
			})
		}
	};

	let ret = dir_reader.filter_map(|result| match result {
		Ok(dir_entry) => {
			let path = dir_entry.path();

			if path.is_symlink() || path.is_dir() {
				return None;
			}

			Some(path)
		}
		Err(err) => {
			let msg_res = message(
				conn,
				format!("Failed to inspect a file under: {} ({err})", path.display()),
				MessageType::WARNING,
			);

			if let Err(msg_err) = msg_res {
				error!("Failed to send an error message (dir. read): {msg_err}");
			}

			None
		}
	});

	Ok(ret)
}

pub(crate) fn send_diags(conn: &Connection, uri: Url, diags: Vec<Diagnostic>) {
	let result = conn.sender.send(Message::Notification(Notification {
		method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string(),
		params: serde_json::to_value(PublishDiagnosticsParams {
			uri,
			diagnostics: diags,
			version: None,
		})
		.unwrap(),
	}));

	if let Err(err) = result {
		error!("Failed to send a diagnostic: {err}");
	}
}

#[must_use]
pub(crate) fn syntaxtext_to_string(text: SyntaxText) -> String {
	let mut ret = String::with_capacity(text.len().into());
	append_syntaxtext(&mut ret, text);
	ret
}

pub(crate) fn uri_to_pathbuf(uri: &Url) -> Result<PathBuf, Error> {
	if uri.scheme() != "file" {
		return Err(Error::Process {
			source: None,
			ctx: format!("non-file URI provided: {uri}"),
		});
	}

	match uri.to_file_path() {
		Ok(pb) => pb.canonicalize().map_err(|err| Error::Process {
			source: Some(Box::new(err)),
			ctx: format!("failed to canonicalize file URI: {uri}"),
		}),
		Err(()) => Err(Error::Process {
			source: None,
			ctx: format!("no host attached to file URI: {uri}"),
		}),
	}
}

/// Results are only valid for absolute paths; will always return `false` if
/// either is relative. A path can not be a child of itself; giving two equal
/// paths will also return `false`.
#[allow(unused)]
#[must_use]
pub(crate) fn path_is_child_of(longer: &Path, shorter: &Path) -> bool {
	if longer.is_relative() | shorter.is_relative() {
		return false;
	}

	if longer == shorter {
		return false;
	}

	let mut a_comps = longer.components();

	for comp in shorter.components() {
		if let Some(self_comp) = a_comps.next() {
			if self_comp == comp {
				continue;
			} else {
				return false;
			}
		} else {
			return false;
		}
	}

	true
}

/// Can be created via [`crate::core::Source::diag_builder`].
#[derive(Debug)]
pub(crate) struct DiagBuilder(pub(crate) Diagnostic);

impl DiagBuilder {
	#[must_use]
	pub(crate) fn with_related(mut self, info: DiagnosticRelatedInformation) -> Self {
		match self.0.related_information.as_mut() {
			Some(r) => r.push(info),
			None => self.0.related_information = Some(vec![info]),
		};

		self
	}

	#[must_use]
	pub(crate) fn _with_tag(mut self, tag: DiagnosticTag) -> Self {
		match self.0.tags.as_mut() {
			Some(t) => t.push(tag),
			None => self.0.tags = Some(vec![tag]),
		};

		self
	}
}
