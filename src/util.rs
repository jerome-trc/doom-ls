use std::path::{Path, PathBuf};

use doomfront::rowan::{TextRange, TextSize};
use lsp_types::{Location, Url};

use crate::lines::LineIndex;

pub(crate) fn uri_to_pathbuf(uri: &Url) -> Result<PathBuf, PathError> {
	if uri.scheme() != "file" {
		return Err(PathError::NonFileUri(uri.scheme().to_owned()));
	}

	let ret = match uri.to_file_path() {
		Ok(pb) => pb,
		Err(()) => return Err(PathError::NoHost),
	};

	ret.canonicalize().map_err(PathError::Canonicalize)
}

pub(crate) fn path_to_uri(path: impl AsRef<Path>) -> Result<Url, PathError> {
	Url::parse(&format!("file://{}", path.as_ref().display()))
		.map_err(|err| PathError::UriParse(Box::new(err)))
}

pub(crate) fn make_location(
	lndx: &LineIndex,
	path: &Path,
	pos: TextSize,
	token_len: usize,
) -> Result<Location, PathError> {
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

#[derive(Debug)]
pub(crate) enum PathError {
	Canonicalize(std::io::Error),
	NoHost,
	NonFileUri(String),
	UriParse(Box<dyn std::error::Error + Send + Sync>),
}

impl std::error::Error for PathError {}

impl std::fmt::Display for PathError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Canonicalize(err) => write!(f, "failed to canonicalize a path: {err}"),
			Self::NoHost => write!(f, "URI host is neither empty nor `localhost`"),
			Self::NonFileUri(scheme) => write!(f, "expected scheme `file`, found: {scheme}"),
			Self::UriParse(err) => write!(f, "failed to parse a `file` URI: {err}"),
		}
	}
}
