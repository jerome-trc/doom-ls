use std::{str::FromStr, sync::OnceLock};

use doomfront::{rowan::GreenNode, zdoom};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position};
use regex::Regex;

use crate::core::Source;

pub(crate) fn full(content: &Source, version: zdoom::Version) -> (GreenNode, Vec<Diagnostic>) {
	let (green, errors) = doomfront::parse(
		&content.text,
		doomfront::zdoom::zscript::parse::file,
		doomfront::zdoom::lex::Context { version },
	)
	.into_inner();

	let mut diags = Vec::with_capacity(errors.len());

	for error in errors {
		let offs_range = error.found().text_range();
		let start_lc = content.lines.line_col(offs_range.start());
		let end_lc = content.lines.line_col(offs_range.end());

		diags.push(Diagnostic {
			range: lsp_types::Range {
				start: Position {
					line: start_lc.line,
					character: start_lc.col,
				},
				end: Position {
					line: end_lc.line,
					character: end_lc.col,
				},
			},
			severity: Some(DiagnosticSeverity::ERROR),
			code: None,
			code_description: None,
			source: Some("doomls-parser".to_string()),
			message: {
				let mut msg = "Expected one of the following:".to_string();

				for expected in error.expected() {
					msg.push_str("\r\n");
					msg.push_str("- ");
					msg.push_str(expected);
				}

				msg
			},
			related_information: None,
			tags: None,
			data: None,
		});
	}

	(green, diags)
}

pub(crate) fn resolve_version(src: &Source) -> Result<zdoom::Version, Diagnostic> {
	static VERSION_RGX: OnceLock<Regex> = OnceLock::new();

	#[must_use]
	fn version_regex_init() -> Regex {
		Regex::new("(?i)version[\0- ]*\"([0-9]+\\.[0-9]+(\\.[0-9]+)?)\"").unwrap()
	}

	let rgx = VERSION_RGX.get_or_init(version_regex_init);

	let Some(caps) = rgx.captures(&src.text) else {
		return Ok(zdoom::Version::V2_4_0);
	};

	let Some(cap0) = caps.get(1) else {
		return Err(Diagnostic {
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
			severity: Some(DiagnosticSeverity::ERROR),
			code: None,
			code_description: None,
			source: Some("doomls".to_string()),
			message: "bad version directive".to_string(),
			related_information: None,
			tags: None,
			data: None,
		});
	};

	let Ok(vers) = zdoom::Version::from_str(cap0.as_str()) else {
		return Err(Diagnostic {
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
			severity: Some(DiagnosticSeverity::ERROR),
			code: None,
			code_description: None,
			source: Some("doomls".to_string()),
			message: "bad version directive".to_string(),
			related_information: None,
			tags: None,
			data: None,
		});
	};

	Ok(vers)
}
