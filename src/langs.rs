//! Every language supported by DoomLS gets a module subtree under this module.

pub(crate) mod cvarinfo;
pub(crate) mod zscript;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LangId {
	Unknown,

	CVarInfo,
	ZScript,
}
