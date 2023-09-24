//! # DoomLS
//!
//! A language server covering domain-specific languages written for Doom's source ports.

fn main() -> Result<(), ErrorBox> {
	Ok(())
}

pub(crate) type ErrorBox = Box<dyn std::error::Error + Send + Sync>;
