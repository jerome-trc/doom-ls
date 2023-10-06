use crossbeam::atomic::AtomicCell;
use triomphe::Arc;

use crate::{
	core::{WorkContext, WorkPhase, WorkTracker},
	setup,
};

#[test]
fn refresh() {
	// Improve clarity of panic messages.
	rayon::ThreadPoolBuilder::new()
		.thread_name(|i| format!("doom-ls_global{i}"))
		.num_threads(1)
		.build_global()
		.unwrap();

	setup::logging();

	let (conn1, _conn2) = lsp_server::Connection::memory();

	let Some(mut core) = setup::test_core(&conn1) else {
		return;
	};

	core.reparse_dirty(&conn1);

	// assert!(core.pending.sema_invalid);
	// assert!(core.pending.malformed.is_empty());

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
	let _ = core.finish_refresh(&conn1);
}
