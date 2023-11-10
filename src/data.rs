use std::time::Instant;

use doomfront::rowan::{SyntaxKind, TextRange};
use lsp_types::SymbolKind;

use crate::{
	arena::APtr,
	core::Scope,
	intern::{NameInterner, NsName, PathIx},
	langs::{self, LangId},
	FxDashMapRef,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FileSpan {
	pub(crate) file_id: PathIx,
	pub(crate) span: TextRange,
}

impl PartialOrd for FileSpan {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for FileSpan {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		match self.file_id.cmp(&other.file_id) {
			core::cmp::Ordering::Equal => {}
			ord => return ord,
		}

		Ord::cmp(
			&(u32::from(self.span.start()), u32::from(self.span.end())),
			&(u32::from(other.span.start()), u32::from(other.span.end())),
		)
	}
}

/// To mitigate chances of misusing [`FileSpan`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub(crate) struct SymbolId(pub(crate) FileSpan);

impl std::ops::Deref for SymbolId {
	type Target = FileSpan;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

#[derive(Debug)]
pub(crate) enum Symbol {
	User(UserSymbol),
	Internal(InternalSymbol),
}

impl Symbol {
	#[must_use]
	pub(crate) fn as_user(&self) -> Option<&UserSymbol> {
		match self {
			Self::User(u) => Some(u),
			Self::Internal(_) => None,
		}
	}

	#[must_use]
	pub(crate) fn as_internal(&self) -> Option<&InternalSymbol> {
		match self {
			Self::Internal(i) => Some(i),
			Self::User(_) => None,
		}
	}

	#[must_use]
	pub(crate) fn lang(&self) -> LangId {
		match self {
			Self::User(u) => u.lang,
			Self::Internal(i) => i.lang,
		}
	}
}

#[derive(Debug)]
pub(crate) struct UserSymbol {
	pub(crate) id: SymbolId,
	/// An index into [`WorkingWorld::projects`].
	pub(crate) project: u8,
	pub(crate) lang: LangId,
	pub(crate) name: NsName,
	pub(crate) syn: SyntaxKind,
	pub(crate) scope: ScopePtr,
	pub(crate) def: DefPtr,
}

impl UserSymbol {
	#[must_use]
	pub(crate) fn definition(&self) -> Option<&Definition> {
		self.def.as_ref()
	}

	#[must_use]
	pub(crate) fn lsp_kind(&self) -> SymbolKind {
		match self.def.as_ref() {
			Some(Definition::ZScript(datum)) => langs::zscript::help::lsp_kind(self, datum),
			Some(Definition::_CVarInfo(_)) => SymbolKind::VARIABLE,
			None => SymbolKind::NULL,
		}
	}
}

impl Drop for UserSymbol {
	fn drop(&mut self) {
		if let Some(ptr) = self.def.as_ptr() {
			unsafe {
				std::ptr::drop_in_place(ptr.as_ptr());
			}
		}
	}
}

#[derive(Debug)]
pub(crate) struct InternalSymbol {
	pub(crate) lang: LangId,
	pub(crate) name: &'static str,
	pub(crate) decl: &'static str,
	pub(crate) docs: &'static [&'static str],
	pub(crate) scope: Option<Scope>,
	pub(crate) def: Definition,
}

#[derive(Debug)]
pub(crate) enum Definition {
	_CVarInfo(langs::cvarinfo::sema::Datum),
	ZScript(langs::zscript::sema::Datum),
}

#[derive(Debug)]
pub(crate) enum SymGraphVal {
	Symbol(SymPtr),
	Symbols(Vec<SymPtr>),
	References(Vec<FileSpan>),
}

impl SymGraphVal {
	#[must_use]
	pub(crate) fn as_symbol(&self) -> Option<&SymPtr> {
		match self {
			Self::Symbol(sym_ptr) => Some(sym_ptr),
			_ => None,
		}
	}
}

/// A key into [`ReadyWorld::sym_graph`]/[`WorkingWorld::sym_graph`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum SymGraphKey {
	/// The value is the direct ancestor of a ZScript or DECORATE class.
	ParentOf(SymPtr),
	/// The value is the set of all ancestors of a ZScript or DECORATE class.
	ChildrenOf(SymPtr),

	/// The value is the set of all mixin classes applied to a ZScript class.
	Mixins(SymPtr),
	/// The value is the set of all ZScript classes which apply a mixin.
	MixinRefs(SymPtr),

	/// The value is the set of all members of a ZScript or DECORATE aggregate.
	Members(SymPtr),
	/// The value is the ZScript or DECORATE aggregate which has this symbol as a member.
	Holder(SymPtr),

	/// The value is the ZScript abstract or virtual function.
	PrototypeFor(SymPtr),
	/// The value is the set of all overrides of this ZScript abstract or virtual function.
	OverrideOf(SymPtr),

	/// The value is the symbol referred to by the token at this file-span.
	/// This span does not necessarily have to map to a token; for example,
	/// a ZScript string literal may have multiple LANGUAGE ID references in it.
	Reference(FileSpan),
	/// The value is a [`SymGraphValue::References`]; all spans referring to this symbol.
	/// These spans do not necessarily have to each map to a token; for example,
	/// a ZScript string literal may have multiple LANGUAGE ID references in it.
	Referred(SymPtr),
}

#[derive(Debug)]
pub(crate) struct InternalDb {
	pub(crate) global: Scope,
	#[allow(unused)]
	pub(crate) bump: bumpalo::Bump,
	// Cache pointers to symbols which are referenced often
	// to avoid having to look them up.
	pub(crate) cache_zscript: langs::zscript::internal::Cache,
}

impl InternalDb {
	#[must_use]
	pub(crate) fn new(names: &NameInterner) -> Self {
		let start_time = Instant::now();

		let mut globals = Scope::default();
		let mut bump = bumpalo::Bump::new();
		let cache_zscript = langs::zscript::internal::register(names, &mut globals, &mut bump);

		tracing::debug!(
			"Internal symbol database initialized in {}ms.",
			start_time.elapsed().as_millis()
		);

		Self {
			global: globals,
			bump,
			cache_zscript,
		}
	}
}

// SAFETY: the `bumpalo::Bump` within is only invoked in `Self::new`.
unsafe impl Sync for InternalDb {}

pub(crate) type SymPtr = APtr<Symbol>;
pub(crate) type DefPtr = APtr<Definition>;
pub(crate) type ScopePtr = APtr<Scope>;

pub(crate) type SymGraphRef<'m> = FxDashMapRef<'m, SymGraphKey, SymGraphVal>;
