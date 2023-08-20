use std::path::{Path, PathBuf};

use doomfront::{
	rowan::{SyntaxNode, SyntaxText, TextRange, TextSize},
	LangExt,
};
use lsp_types::{Location, Url};

use crate::{lines::LineIndex, Error};

/// `whitespace` is used to ensure that whitespace tokens have their contents
/// replaced with a single space character.
pub(crate) fn append_descendant_tokens<L: LangExt>(
	string: &mut String,
	node: &SyntaxNode<L>,
	whitespace: L::Kind,
) {
	for desc in node.descendants_with_tokens() {
		let Some(token) = desc.into_token() else { continue; };

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

#[must_use]
pub(crate) fn syntaxtext_to_string(text: SyntaxText) -> String {
	let mut ret = String::with_capacity(text.len().into());
	append_syntaxtext(&mut ret, text);
	ret
}

pub(crate) fn parse_uri(string: &str) -> Result<PathBuf, Error> {
	let uri = Url::parse(string).map_err(|err| {
		Error::Process {
			source: Some(Box::new(err)),
			ctx: "parsing URI".to_string(),
		}
	})?;

	uri_to_pathbuf(&uri)
}

pub(crate) fn uri_to_pathbuf(uri: &Url) -> Result<PathBuf, Error> {
	if uri.scheme() != "file" {
		return Err(Error::Process {
			source: None,
			ctx: format!("non-file URI provided: {uri}"),
		});
	}

	let ret = match uri.to_file_path() {
		Ok(pb) => pb,
		Err(()) => {
			return Err(Error::Process {
				source: None,
				ctx: format!("no host attached to file URI: {uri}"),
			})
		}
	};

	ret.canonicalize().map_err(|err| Error::Process {
		source: Some(Box::new(err)),
		ctx: format!("failed to canonicalize file URI: {uri}"),
	})
}

pub(crate) fn path_to_uri(path: impl AsRef<Path>) -> Result<Url, Error> {
	let path = path.as_ref();

	Url::parse(&format!("file://{}", path.display())).map_err(|err| Error::Process {
		source: Some(Box::new(err)),
		ctx: format!("failed to parse a file URI from path: {}", path.display()),
	})
}

pub(crate) fn make_location(
	lndx: &LineIndex,
	path: &Path,
	pos: TextSize,
	token_len: usize,
) -> Result<Location, Error> {
	let uri = path_to_uri(path)?;
	let start_lc = lndx.line_col(pos);
	let end_lc = lndx.line_col(pos + TextSize::from(token_len as u32));
	let start = lsp_types::Position::from(start_lc);
	let end = lsp_types::Position::from(end_lc);

	Ok(Location {
		uri,
		range: lsp_types::Range { start, end },
	})
}

#[must_use]
pub(crate) fn make_range(lndx: &LineIndex, range: TextRange) -> lsp_types::Range {
	lsp_types::Range {
		start: lsp_types::Position::from(lndx.line_col(range.start())),
		end: lsp_types::Position::from(lndx.line_col(range.end())),
	}
}

/// Results are only valid for absolute paths; will always return `false` if
/// either is relative. A path can not be a child of itself; giving two equal
/// paths will also return `false`.
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
