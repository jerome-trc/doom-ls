//! The ZScript frontend's third phase; definition.
//!
//! Semantic checks on function bodies, actor defaults, and actor state machines
//! happen here. Initializers for symbolic constants are also checked and evaluated.
//!
//! Additionally, name-to-symbol references are added to the graph.

pub(crate) mod class;
pub(crate) mod function;
