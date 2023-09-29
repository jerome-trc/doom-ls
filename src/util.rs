//! Helper symbols that don't belong anywhere else.

use std::path::{Path, PathBuf};

use doomfront::{
	rowan::{SyntaxNode, SyntaxText},
	LangExt,
};
use lsp_server::{Connection, Message, Notification, RequestId, Response};
use lsp_types::{
	notification::{Notification as LspNotification, ShowMessage},
	Diagnostic, DiagnosticRelatedInformation, DiagnosticTag, MessageType, ShowMessageParams, Url,
};

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
