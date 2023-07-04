use std::path::{Path, PathBuf};

use lsp_types::Url;

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

#[allow(unused)]
pub(crate) fn path_to_uri(path: impl AsRef<Path>) -> Result<Url, PathError> {
	Url::parse(&format!("file://{}", path.as_ref().display()))
		.map_err(|err| PathError::UriParse(Box::new(err)))
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
