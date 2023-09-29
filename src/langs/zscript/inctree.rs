//! An include tree walking routine.

use std::{path::Path, str::FromStr, sync::OnceLock};

use doomfront::{
	rowan::{ast::AstNode, GreenNode, TextRange},
	zdoom::{
		self,
		zscript::{ast, Syn, SyntaxNode},
	},
};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position};
use rayon::prelude::*;
use regex::Regex;

use crate::{
	core::{Core, Project, Source},
	intern::{PathInterner, PathIx},
	langs::LangId,
	FxDashMap,
};

pub(crate) fn walk(core: &mut Core) {
	#[derive(Debug, Clone, Copy)]
	struct Walker<'a> {
		paths: &'a PathInterner,
		project: &'a Project,
		output: &'a FxDashMap<PathIx, Output>,
		diags: &'a FxDashMap<PathIx, Vec<Diagnostic>>,
		src: &'a Source,
	}

	#[derive(Debug)]
	struct Output {
		green: Option<GreenNode>,
		includer: PathIx,
	}

	impl Walker<'_> {
		fn raise(&self, message: String, span: TextRange) {
			let mut diags = self.diags.entry(self.src.id).or_insert(vec![]);

			let start_lc = self.src.lines.line_col(span.start());
			let end_lc = self.src.lines.line_col(span.end());

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
				source: Some("doomls".to_string()),
				message,
				related_information: None,
				tags: None,
				data: None,
			});
		}
	}

	fn recur(walker: Walker, green: GreenNode) {
		green
			.children()
			.filter_map(|n_or_t| {
				n_or_t
					.into_node()
					.filter(|node| node.kind() == Syn::IncludeDirective.into())
					.map(|gnd| gnd.to_owned())
			})
			.par_bridge()
			.for_each(|green| {
				let cursor = SyntaxNode::new_root(green);
				let directive = ast::IncludeDirective::cast(cursor).unwrap();
				let lit_tok = directive.argument().unwrap();

				let Some(string) = lit_tok.string() else {
					walker.raise(
						"expected a string".to_string(),
						lit_tok.syntax().text_range(),
					);

					return;
				};

				let inc_path = Path::new(string);

				let full_path = if inc_path.is_relative() {
					walker
						.paths
						.resolve(walker.src.id)
						.parent()
						.unwrap()
						.join(inc_path)
				} else {
					walker.project.root.join(inc_path)
				};

				let included_id = walker.paths.intern(&full_path);

				let Some(src) = walker.project.files.get(&included_id) else {
					walker.raise(
						format!("file does not exist: {}", full_path.display()),
						lit_tok.syntax().text_range(),
					);

					return;
				};

				let green = if let Some(g) = src.green.as_ref() {
					walker.output.insert(
						included_id,
						Output {
							green: None,
							includer: walker.src.id,
						},
					);

					g.clone()
				} else {
					let (green, mut diags) =
						super::parse::full(src, walker.project.zscript.version);

					walker.output.insert(
						included_id,
						Output {
							green: Some(green.clone()),
							includer: walker.src.id,
						},
					);

					if !diags.is_empty() {
						walker
							.diags
							.entry(included_id)
							.or_insert(vec![])
							.append(&mut diags);
					}

					green
				};

				recur(Walker { src, ..walker }, green);
			});
	}

	static VERSION_RGX: OnceLock<Regex> = OnceLock::new();

	#[must_use]
	fn version_regex_init() -> Regex {
		Regex::new("(?i)version[\0- ]*\"([0-9]+\\.[0-9]+(\\.[0-9]+)?)\"").unwrap()
	}

	for project in &mut core.pending.projects {
		let Some(root_id) = project.zscript.root else {
			continue;
		};

		let src = project.files.get_mut(&root_id).unwrap();

		let rgx = VERSION_RGX.get_or_init(version_regex_init);

		if let Some(caps) = rgx.captures(&src.text) {
			let Some(cap0) = caps.get(1) else {
				let diags = core.pending.malformed.entry(root_id).or_insert(vec![]);

				diags.push(Diagnostic {
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

				continue;
			};

			let Ok(vers) = zdoom::Version::from_str(cap0.as_str()) else {
				let diags = core.pending.malformed.entry(root_id).or_insert(vec![]);

				diags.push(Diagnostic {
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

				continue;
			};

			project.zscript.version = vers;
		};

		if src.green.is_none() {
			let (green, mut diags) = super::parse::full(src, project.zscript.version);
			src.green = Some(green);

			if !diags.is_empty() {
				let diags_entry = core.pending.malformed.entry(root_id).or_insert(vec![]);
				diags_entry.append(&mut diags);
			}
		}

		let src = project.files.get(&root_id).unwrap();
		let green = src.green.as_ref().unwrap().clone();
		let output = FxDashMap::default();
		let diags = FxDashMap::default();

		recur(
			Walker {
				paths: &core.paths,
				project,
				src,
				output: &output,
				diags: &diags,
			},
			green.clone(),
		);

		project.zscript.includes.clear();

		for (included, op) in output {
			project.zscript.includes.insert(included, op.includer);

			if let Some(green) = op.green {
				let src = project.files.get_mut(&included).unwrap();
				src.lang = LangId::ZScript;
				debug_assert!(src.green.is_none());
				src.green = Some(green);
			}
		}

		for (file_id, mut d) in diags {
			debug_assert!(!d.is_empty());
			let diags_entry = core.pending.malformed.entry(file_id).or_insert(vec![]);
			diags_entry.append(&mut d);
		}
	}
}
