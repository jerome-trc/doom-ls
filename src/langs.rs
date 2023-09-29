//! Every language supported by DoomLS gets a module subtree under this module.

pub(crate) mod cvarinfo;
pub(crate) mod zscript;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum LangId {
	Unknown,

	CVarInfo,
	Decorate,
	ZScript,
}

impl LangId {
	/// For use in LSP communication, not the user interface.
	#[must_use]
	pub(crate) fn to_str(self) -> &'static str {
		match self {
			LangId::Unknown => unreachable!(),
			LangId::CVarInfo => "cvarinfo",
			LangId::Decorate => "decorate",
			LangId::ZScript => "zscript",
		}
	}
}
