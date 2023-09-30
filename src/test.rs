use std::path::Path;

use crossbeam::atomic::AtomicCell;
use lsp_types::MessageType;
use triomphe::Arc;

use crate::{
	core::{Core, Project, Source, WorkContext, WorkPhase, WorkTracker},
	langs::{self, LangId},
	lines::LineIndex,
	setup, util, FxHamt,
};

#[test]
fn refresh() {
	setup::logging();

	let (conn1, _conn2) = lsp_server::Connection::memory();
	let mut core = Core::default();
	let project_path = Path::new("/home/jerome/Data/doom_mod_dev/biomorph/biomorph");

	let mut project = Project {
		root: project_path.to_owned(),
		files: FxHamt::default(),
		zscript: langs::zscript::IncludeTree::default(),
	};

	let read_dir = std::fs::read_dir(project_path).unwrap();

	for result in read_dir {
		let Ok(rde) = result else {
			util::message(
				&conn1,
				format!("Failed to inspect file: {}", project_path.display()),
				MessageType::WARNING,
			)
			.unwrap();

			continue;
		};

		let path = rde.path();

		if path.is_symlink() {
			continue;
		} else if path.is_dir() {
			setup::walk_dir(&mut core, &conn1, &mut project, path).unwrap();
			continue;
		}

		let Some(fstem) = path.file_stem() else {
			continue;
		};

		let text = match std::fs::read_to_string(&path) {
			Ok(t) => t,
			Err(err) => {
				if err.kind() != std::io::ErrorKind::InvalidData {
					util::message(
						&conn1,
						format!("Failed to read file in project: {}", path.display()),
						MessageType::WARNING,
					)
					.unwrap();
				}

				continue;
			}
		};

		let file_id = core.paths.intern(&path);
		let lines = LineIndex::new(&text);

		project.files.insert(
			file_id,
			Source {
				id: file_id,
				lang: LangId::Unknown,
				text,
				lines,
				green: None,
			},
		);

		if fstem.eq_ignore_ascii_case("zscript") {
			project.zscript.root = Some(file_id);
		}
	}

	core.pending.projects.push(project);

	langs::zscript::inctree::walk(&mut core);

	core.reparse_dirty(&conn1);

	assert!(core.pending.sema_invalid);
	assert!(core.pending.malformed.is_empty());

	let tracker = Arc::new(WorkTracker {
		phase: AtomicCell::new(WorkPhase::default()),
	});

	let working = core.working.clone();
	let projects = core.pending.projects.clone();

	rayon::spawn(move || {
		let mut guard = working.lock();
		guard.refresh(WorkContext { tracker, projects });
		drop(guard);
	});

	while !core.working.is_locked() {
		std::thread::sleep(std::time::Duration::from_millis(5));
	}

	let guard = core.working.lock();
	drop(guard);
	core.finish_refresh(&conn1);
}
