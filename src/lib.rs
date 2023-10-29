//! # DoomLS Core
//!
//! The common code that underpins the DoomLS binary, in a separate library so
//! that benchmarks have something to link against.

use std::hash::BuildHasherDefault;

use dashmap::{DashMap, DashSet};
use rustc_hash::FxHasher;

pub mod error;

pub type ErrorBox = Box<dyn std::error::Error + Send + Sync>;
pub type FxDashMap<K, V> = DashMap<K, V, BuildHasherDefault<FxHasher>>;
pub type FxDashSet<K> = DashSet<K, BuildHasherDefault<FxHasher>>;
pub type FxDashView<K, V> = dashmap::ReadOnlyView<K, V, BuildHasherDefault<FxHasher>>;
pub type FxHamt<K, V> = im::HashMap<K, V, BuildHasherDefault<FxHasher>>;
pub type UnitResult = Result<(), error::Error>;

pub type FxDashMapRef<'m, K, V> =
	dashmap::mapref::one::Ref<'m, K, V, BuildHasherDefault<FxHasher>>;
pub type FxDashMapRefMut<'m, K, V> =
	dashmap::mapref::one::RefMut<'m, K, V, BuildHasherDefault<FxHasher>>;
