//! Functions related to manipulating include trees.

use std::path::{Path, PathBuf};

use doomfront::{
	rowan::{ast::AstNode, GreenNode},
	zdoom::{
		ast::LitToken,
		zscript::{ast, Syn, SyntaxNode},
	},
};
use lsp_types::{Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Url};
use petgraph::prelude::DiGraphMap;
use rayon::prelude::*;

use crate::{
	core::{Core, Project, Source},
	intern::{PathInterner, PathIx},
	langs::LangId,
	FxDashMap, FxHamt,
};

/// For building an include tree from nothing during initial startup.
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
		directive_span: lsp_types::Range,
	}

	impl Walker<'_> {
		fn raise(&self, diag: Diagnostic) {
			let mut diags = self.diags.entry(self.src.id).or_insert(vec![]);
			diags.push(diag);
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
				let lit = directive.argument().unwrap();

				let Some(string) = lit.string() else {
					walker.raise(
						walker
							.src
							.diag_builder(
								lit.syntax().text_range(),
								DiagnosticSeverity::ERROR,
								"expected a string argument".to_string(),
							)
							.0,
					);

					return;
				};

				let full_path = match compose_include_path(
					walker.paths,
					&walker.project.root,
					walker.src,
					&lit,
					string,
				) {
					Ok(p) => p,
					Err(diag) => {
						walker.raise(diag);
						return;
					}
				};

				let included_id = walker.paths.intern(&full_path);

				let Some(src) = walker.project.files.get(&included_id) else {
					walker.raise(
						walker
							.src
							.diag_builder(
								lit.syntax().text_range(),
								DiagnosticSeverity::ERROR,
								format!("included file does not exist: {}", full_path.display()),
							)
							.0,
					);

					return;
				};

				let directive_span = walker.src.make_range(directive.syntax().text_range());

				let green = if let Some(g) = src.green.as_ref() {
					walker.output.insert(
						included_id,
						Output {
							green: None,
							includer: walker.src.id,
							directive_span,
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
							directive_span,
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

	for project in &mut core.pending.projects {
		let Some(root_id) = project.zscript.root else {
			continue;
		};

		let src = project.files.get_mut(&root_id).unwrap();
		src.lang = LangId::ZScript;

		match super::parse::resolve_version(src) {
			Ok(vers) => {
				project.zscript.version = vers;
			}
			Err(diag) => {
				core.pending
					.malformed
					.entry(root_id)
					.or_insert(vec![])
					.push(diag);
			}
		}

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
			project
				.zscript
				.includes
				.add_edge(op.includer, included, op.directive_span);

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

pub(crate) fn get_includes(
	paths: &PathInterner,
	project_root: &Path,
	files: FxHamt<PathIx, Source>,
	graph: &mut DiGraphMap<PathIx, lsp_types::Range>,
	src: &Source,
	diags: &mut Vec<Diagnostic>,
) {
	let cursor = SyntaxNode::new_root(src.green.as_ref().unwrap().clone());

	for top in cursor.children().filter_map(ast::TopLevel::cast) {
		let ast::TopLevel::Include(directive) = top else {
			continue;
		};

		let Ok(lit) = directive.argument() else {
			diags.push(
				src.diag_builder(
					directive.syntax().text_range(),
					DiagnosticSeverity::ERROR,
					"expected a string argument".to_string(),
				)
				.0,
			);

			continue;
		};

		let Some(string) = lit.string() else {
			diags.push(
				src.diag_builder(
					lit.syntax().text_range(),
					DiagnosticSeverity::ERROR,
					"expected a string argument".to_string(),
				)
				.0,
			);

			return;
		};

		let full_path = match compose_include_path(paths, project_root, src, &lit, string) {
			Ok(p) => p,
			Err(diag) => {
				diags.push(diag);
				return;
			}
		};

		let included_id = paths.intern(&full_path);

		if !files.contains_key(&included_id) {
			diags.push(
				src.diag_builder(
					lit.syntax().text_range(),
					DiagnosticSeverity::ERROR,
					format!("included file does not exist: {}", full_path.display()),
				)
				.0,
			);

			return;
		};

		if let Some((_, included, directive_span)) = graph
			.edges_directed(included_id, petgraph::Direction::Incoming)
			.next()
		{
			let other_path = paths.resolve(included);
			let other_uri = Url::from_file_path(other_path).unwrap();

			let other_loc = Location {
				uri: other_uri,
				range: *directive_span,
			};

			let diag_builder = src
				.diag_builder(
					lit.syntax().text_range(),
					DiagnosticSeverity::WARNING,
					format!("file has already been included: {}", full_path.display()),
				)
				.with_related(DiagnosticRelatedInformation {
					location: other_loc,
					message: "file has already been included from here".to_string(),
				});

			diags.push(diag_builder.0);
		}

		let directive_span = src.make_range(directive.syntax().text_range());
		graph.add_edge(src.id, included_id, directive_span);
	}
}

pub(crate) fn compose_include_path(
	paths: &PathInterner,
	project_root: &Path,
	src: &Source,
	lit: &LitToken<Syn>,
	lit_str: &str,
) -> Result<PathBuf, Diagnostic> {
	let inc_path = Path::new(lit_str);

	let mut inc_path_components = inc_path.components();

	let inc_path_absolute = matches!(
		inc_path_components.next(),
		Some(std::path::Component::RootDir)
	);

	let full_path = if inc_path_absolute {
		project_root.join(inc_path_components.collect::<PathBuf>())
	} else {
		let parent = paths.resolve(src.id).parent().unwrap();
		let joined = parent.join(inc_path);

		match joined.canonicalize() {
			Ok(p) => p,
			Err(_) => {
				return Err(src
					.diag_builder(
						lit.syntax().text_range(),
						DiagnosticSeverity::ERROR,
						format!(
							"malformed include path or non-existent file: {}",
							joined.display(),
						),
					)
					.0);
			}
		}
	};

	Ok(full_path)
}
