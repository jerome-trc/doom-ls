use doomfront::{rowan::GreenNode, zdoom};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position};

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
