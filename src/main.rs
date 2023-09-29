//! # DoomLS
//!
//! A language server covering domain-specific languages written for Doom's source ports.

pub(crate) mod core;
pub(crate) mod error;
pub(crate) mod frontend;
pub(crate) mod intern;
pub(crate) mod langs;
pub(crate) mod lines;
pub(crate) mod notif;
pub(crate) mod request;
pub(crate) mod semtok;
pub(crate) mod setup;
pub(crate) mod util;

use std::{
	hash::BuildHasherDefault,
	ops::ControlFlow,
	time::{Duration, Instant},
};

use crossbeam::{atomic::AtomicCell, channel::RecvTimeoutError};
use dashmap::DashMap;
use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
	notification::{Notification as LspNotification, ShowMessage},
	request::{Request as LspRequest, WorkspaceConfiguration},
	ConfigurationItem, ConfigurationParams, InitializeParams, MessageType, ShowMessageParams,
};
use rustc_hash::FxHasher;
use tracing::{error, info};
use triomphe::Arc;

use core::{Core, WorkContext, WorkTracker};
use error::Error;

use crate::core::WorkPhase;

fn main() -> Result<(), ErrorBox> {
	setup::logging();
	info!("Initializing...");

	rayon::ThreadPoolBuilder::new()
		.thread_name(|i| format!("doom-ls_global{i}"))
		.num_threads(if let Ok(p) = std::thread::available_parallelism() {
			(p.get() / 2).max(1)
		} else {
			0 // Allow Rayon to decide for itself.
		})
		.build_global()?;

	info!("Global thread pool constructed.");
	let (conn, threads) = lsp_server::Connection::stdio();
	let params = conn.initialize(serde_json::to_value(setup::capabilities())?)?;
	let _: InitializeParams = serde_json::from_value(params).unwrap();

	conn.sender.send(Message::Request(Request {
		id: RequestId::from(0),
		method: WorkspaceConfiguration::METHOD.to_string(),
		params: serde_json::to_value(ConfigurationParams {
			items: vec![ConfigurationItem {
				scope_uri: None,
				section: Some("doomls".to_string()),
			}],
		})
		.unwrap(),
	}))?;

	let start_time = Instant::now();
	let core;

	loop {
		if start_time.elapsed().as_secs() > 30 {
			error!("Did not receive configuration from client; terminating.");
			drop(conn);
			threads.join()?;
			return Ok(());
		}

		let msg = match conn.receiver.recv_timeout(Duration::from_millis(500)) {
			Ok(m) => m,
			Err(err) => match err {
				RecvTimeoutError::Timeout => continue,
				RecvTimeoutError::Disconnected => return Ok(()),
			},
		};

		match msg {
			Message::Response(resp) => {
				if resp.id != RequestId::from(0) {
					continue;
				}

				core = setup::core(&conn, resp)?;
				break;
			}
			Message::Request(req) => {
				if conn.handle_shutdown(&req).map_err(Error::from)? {
					info!("Server shutting down...");
					drop(conn);
					threads.join()?;
					return Ok(());
				}
			}
			Message::Notification(_) => {}
		}
	}

	info!("Initial configuration received.");
	main_loop(core, conn)?;
	threads.join()?;
	info!("Server shutting down...");
	Ok(())
}

fn main_loop(mut core: Core, conn: Connection) -> UnitResult {
	const TIMEOUT: Duration = Duration::from_millis(250);
	let mut work_tracker: Option<Arc<WorkTracker>> = None;

	loop {
		if core.pending.last_change.elapsed() > Duration::from_millis(250) {
			core.reparse_dirty(&conn);
		}

		if work_tracker.is_some() {
			core.finish_refresh(&conn);
			let _ = work_tracker.take().unwrap();
		} else if core.should_refresh() {
			tracing::debug!("Dispatching a workspace refresh.");

			let tracker = Arc::new(WorkTracker {
				phase: AtomicCell::new(WorkPhase::default()),
			});

			let working = core.working.clone();
			let projects = core.pending.projects.clone();
			core.pending.sema_invalid = false;

			work_tracker = Some(tracker.clone());

			rayon::spawn(move || {
				let mut guard = working.lock();
				guard.refresh(WorkContext { tracker, projects });
			});
		}

		let msg = match conn.receiver.recv_timeout(TIMEOUT) {
			Ok(m) => m,
			Err(RecvTimeoutError::Disconnected) => return Ok(()),
			Err(RecvTimeoutError::Timeout) => continue,
		};

		match msg {
			Message::Request(req) => {
				if conn.handle_shutdown(&req).map_err(Error::from)? {
					drop(conn);
					return Ok(());
				}

				process_request(&mut core, &conn, req);
			}
			Message::Response(_) => {
				// TODO
			}
			Message::Notification(notif) => {
				process_notif(&mut core, &conn, notif);
			}
		}
	}
}

fn process_request(core: &mut Core, conn: &Connection, req: Request) {
	match request::handle(core, conn, req) {
		ControlFlow::Break(Err(error)) => {
			match error {
				Error::Send(err) => {
					// Don't bother trying to send an error response if
					// there's reason to believe the channel is compromised.
					error!("{err}");
				}
				Error::Response(resp) => {
					{
						let e = resp.error.as_ref().unwrap();
						error!("{e:#?}");
					}

					let send_res = conn.sender.send(Message::Response(resp));

					if let Err(e) = send_res {
						error!("Failed to send error message: {e}");
					}
				}
				Error::Process { .. } => {}
			}
		}
		ControlFlow::Continue(_) | ControlFlow::Break(_) => {}
	}
}

fn process_notif(core: &mut Core, conn: &Connection, notif: Notification) -> ControlFlow<()> {
	match notif::handle(core, conn, notif) {
		ControlFlow::Break(Err(error)) => {
			match error {
				Error::Send(err) => {
					// Don't bother trying to send an error response if
					// there's reason to believe the channel is compromised.
					error!("{err}");
				}
				err @ Error::Process { .. } => {
					let send_res = conn.sender.send(Message::Notification(Notification {
						method: ShowMessage::METHOD.to_string(),
						params: serde_json::to_value(ShowMessageParams {
							typ: MessageType::ERROR,
							message: err.to_string(),
						})
						.unwrap(),
					}));

					if let Err(e) = send_res {
						error!("Failed to send error message: {e}");
					}
				}
				Error::Response(_) => unreachable!(),
			}

			ControlFlow::Continue(())
		}
		ControlFlow::Continue(_) => ControlFlow::Continue(()),
		ControlFlow::Break(_) => ControlFlow::Break(()),
	}
}

pub(crate) type ErrorBox = Box<dyn std::error::Error + Send + Sync>;
pub(crate) type FxDashMap<K, V> = DashMap<K, V, BuildHasherDefault<FxHasher>>;
pub(crate) type FxDashView<K, V> = dashmap::ReadOnlyView<K, V, BuildHasherDefault<FxHasher>>;
pub(crate) type FxHamt<K, V> = im::HashMap<K, V, BuildHasherDefault<FxHasher>>;
pub(crate) type UnitResult = Result<(), error::Error>;

#[allow(unused)]
pub(crate) type FxDashMapRef<'m, K, V> =
	dashmap::mapref::one::Ref<'m, K, V, BuildHasherDefault<FxHasher>>;
#[allow(unused)]
pub(crate) type FxDashMapRefMut<'m, K, V> =
	dashmap::mapref::one::RefMut<'m, K, V, BuildHasherDefault<FxHasher>>;
