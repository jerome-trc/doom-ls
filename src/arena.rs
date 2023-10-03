//! [`Arena`] and [`APtr`].

use std::{
	hash::{Hash, Hasher},
	ptr::NonNull,
};

use crossbeam::atomic::AtomicCell;
use parking_lot::{Mutex, MutexGuard};

/// A concurrent bump allocator implemented via mutex-guarded sharding.
#[derive(Debug, Default)]
pub(crate) struct Arena {
	// TODO: there are probably far better sharding schemes out there than this.
	shards: [Mutex<bumpalo::Bump>; 2],
}

impl Arena {
	pub(crate) fn borrow(&self) -> MutexGuard<bumpalo::Bump> {
		if let Some(shard) = self.shards[0].try_lock() {
			shard
		} else {
			self.shards[1].lock()
		}
	}

	#[must_use]
	pub(crate) fn alloc<T>(bump: &mut bumpalo::Bump, obj: T) -> APtr<T> {
		let m = bump.alloc(obj);
		let ret = APtr::<T>::null();
		ret.store(NonNull::new(m as *mut T).unwrap());
		ret
	}

	pub(crate) fn reset(&mut self) {
		for shard in &mut self.shards {
			shard.get_mut().reset();
		}
	}
}

/// An atomic pointer to an allocation in an [`Arena`].
///
/// This type has no safety guarantees on its own. Its soundness relies on the
/// presumption of correct usage by the main loop, which is expected to:
/// - never be exposed to a pointer sourced from anywhere other than the ready world;
/// - never store pointers longer than necessary to service a request or notification,
/// since pointers allocated during a refresh are all immediately invalidated when
/// that refresh is unpacked into the ready world.
///
/// The exception is pointers to internal data (e.g. ZScript's `Object` class),
/// which are initialized at the start of the server's runtime and never freed,
/// so they can be treated as though they were `'static`.
///
/// Does not benefit from null pointer optimization.
#[derive(Debug)]
pub(crate) struct APtr<T>(
	// (RAT): It's weird that there's no way to get
	// null-pointer optimization for an `AtomicPtr`.
	AtomicCell<Option<NonNull<T>>>,
);

impl<T> APtr<T> {
	#[must_use]
	pub(crate) fn null() -> Self {
		Self(AtomicCell::new(None))
	}

	pub(crate) fn store(&self, new: NonNull<T>) {
		self.0.store(Some(new));
	}

	/// Returns `None` if the pointer within is null.
	#[must_use]
	pub(crate) fn as_ref(&self) -> Option<&T> {
		unsafe { self.0.load().map(|nn| nn.as_ref()) }
	}

	/// Returns `None` if the pointer within is null.
	#[must_use]
	pub(crate) unsafe fn as_mut(&mut self) -> Option<&mut T> {
		self.0.load().map(|mut nn| nn.as_mut())
	}

	#[must_use]
	pub(crate) fn as_ptr(&self) -> Option<NonNull<T>> {
		self.0.load()
	}
}

impl<T> std::ops::Deref for APtr<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		self.as_ref().unwrap()
	}
}

impl<T> PartialEq for APtr<T> {
	fn eq(&self, other: &Self) -> bool {
		self.0.load() == other.0.load()
	}
}

impl<T> Eq for APtr<T> {}

impl<T> Hash for APtr<T> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.0.load().hash(state);
	}
}

impl<T> Clone for APtr<T> {
	fn clone(&self) -> Self {
		Self(AtomicCell::new(self.0.load()))
	}
}

unsafe impl<T: Send> Send for APtr<T> {}
unsafe impl<T: Send + Sync> Sync for APtr<T> {}

const _STATIC_ASSERT_APTR_CONSTRAINTS: () = {
	assert!(std::mem::size_of::<APtr<()>>() == std::mem::size_of::<*mut ()>());
	assert!(AtomicCell::<Option<NonNull<()>>>::is_lock_free());
};
