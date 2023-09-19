//! # DoomLS
//!
//! A language server covering domain-specific languages written for Doom's source ports.

use std::marker::PhantomData;

/// A strongly-typed 32-bit index into a collection of `T`s.
/// `T` is meaningless to the program; it is only to add clarity to the code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct IxT<T>(u32, PhantomData<fn(T)>);

fn main() -> Result<(), ErrorBox> {
	Ok(())
}

#[derive(Debug)]
pub(crate) enum Error {}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub(crate) type ErrorBox = Box<dyn std::error::Error + Send + Sync>;
