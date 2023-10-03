//! See [`FrontendContext`].

use doomfront::{
	rowan::{SyntaxNode, TextRange},
	LangExt,
};
use lsp_types::{Location, Url};

use crate::{
	arena::Arena,
	core::{Scope, Source, WorkingWorld},
	data::{
		DefPtr, FileSpan, ScopePtr, SymGraphKey, SymGraphRef, SymGraphVal, SymPtr, Symbol,
		SymbolId, UserSymbol,
	},
	intern::NsName,
	langs::LangId,
	util::DiagBuilder,
};

/// Common data and functions for frontend compilation in a [file][Source].
#[derive(Debug, Clone, Copy)]
pub(crate) struct FrontendContext<'w> {
	pub(crate) inner: &'w WorkingWorld,
	pub(crate) project_ix: usize,
	pub(crate) src: &'w Source,
}

impl<'w> FrontendContext<'w> {
	/// `Ok` contains a pointer to the newly-declared symbol.
	/// `Err` contains a pointer to the symbol that would have been overwritten.
	pub(crate) fn declare_and<L: LangExt>(
		&self,
		outer: &mut Scope,
		ns_name: NsName,
		lang: LangId,
		node: &doomfront::rowan::SyntaxNode<L>,
		mut pre_insert: impl FnMut(&SymPtr),
	) -> Result<SymPtr, SymPtr> {
		let id = SymbolId(FileSpan {
			file_id: self.src.id,
			span: node.text_range(),
		});

		let sym_ptr = match outer.entry(ns_name) {
			im::hashmap::Entry::Vacant(vac) => {
				let sym = Symbol::User(UserSymbol {
					id,
					project: self.project_ix as u8,
					lang,
					syn: L::kind_to_raw(node.kind()),
					name: ns_name,
					scope: ScopePtr::null(),
					def: DefPtr::null(),
				});

				let mut bump = self.arena.borrow();
				let sym_ptr = Arena::alloc(&mut bump, sym);
				pre_insert(&sym_ptr);
				self.symbols.insert(id, sym_ptr.clone());
				vac.insert(sym_ptr.clone());
				sym_ptr
			}
			im::hashmap::Entry::Occupied(occ) => {
				return Err(occ.get().clone());
			}
		};

		Ok(sym_ptr)
	}

	/// `Ok` contains a pointer to the newly-declared symbol.
	/// `Err` contains a pointer to the symbol that would have been overwritten.
	pub(crate) fn declare<L: LangExt>(
		&self,
		outer: &mut Scope,
		ns_name: NsName,
		lang: LangId,
		node: &doomfront::rowan::SyntaxNode<L>,
	) -> Result<SymPtr, SymPtr> {
		self.declare_and(outer, ns_name, lang, node, |_| {})
	}

	/// A returned `Some` contains the index of the overriden symbol.
	pub(crate) fn decl_override<L: LangExt>(
		&self,
		outer: &mut Scope,
		ns_name: NsName,
		lang: LangId,
		node: &doomfront::rowan::SyntaxNode<L>,
	) -> (SymPtr, Option<SymPtr>) {
		let id = SymbolId(FileSpan {
			file_id: self.src.id,
			span: node.text_range(),
		});

		let sym = Symbol::User(UserSymbol {
			id,
			project: self.project_ix as u8,
			lang,
			syn: L::kind_to_raw(node.kind()),
			name: ns_name,
			scope: ScopePtr::null(),
			def: DefPtr::null(),
		});

		let mut bump = self.arena.borrow();
		let sym_ptr = Arena::alloc(&mut bump, sym);

		self.symbols.insert(id, sym_ptr.clone());

		(sym_ptr.clone(), outer.insert(ns_name, sym_ptr))
	}

	#[must_use]
	pub(crate) fn get_symbol(&self, src: &Source, span: TextRange) -> Option<SymPtr> {
		self.symbols
			.get(&SymbolId(FileSpan {
				file_id: src.id,
				span,
			}))
			.map(|r| r.value().clone())
	}

	pub(crate) fn make_member(&self, member: SymPtr, holder: SymPtr) {
		self.sym_graph.insert(
			SymGraphKey::Holder(member.clone()),
			SymGraphVal::Symbol(holder.clone()),
		);

		let mut refmut = self
			.sym_graph
			.entry(SymGraphKey::Members(holder))
			.or_insert(SymGraphVal::Symbols(vec![]));

		let SymGraphVal::Symbols(members) = refmut.value_mut() else {
			unreachable!()
		};

		members.push(member);
	}

	pub(crate) fn make_ref_to(&self, span: TextRange, sym_ptr: SymPtr) {
		let fspan = FileSpan {
			file_id: self.src.id,
			span,
		};

		self.sym_graph.insert(
			SymGraphKey::Reference(fspan),
			SymGraphVal::Symbol(sym_ptr.clone()),
		);

		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::Referred(sym_ptr))
			.or_insert(SymGraphVal::References(vec![]));

		let SymGraphVal::References(refs) = sgn.value_mut() else {
			unreachable!();
		};

		refs.push(fspan);
	}

	pub(crate) fn make_child_of(&self, parent_ptr: SymPtr, child_ptr: SymPtr) {
		self.sym_graph.insert(
			SymGraphKey::ParentOf(child_ptr.clone()),
			SymGraphVal::Symbol(parent_ptr.clone()),
		);

		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::ChildrenOf(parent_ptr))
			.or_insert(SymGraphVal::Symbols(vec![]));

		let SymGraphVal::Symbols(children) = sgn.value_mut() else {
			unreachable!();
		};

		children.push(child_ptr);
	}

	pub(crate) fn make_override_of(&self, prototype_ptr: SymPtr, override_ptr: SymPtr) {
		self.sym_graph.insert(
			SymGraphKey::PrototypeFor(override_ptr.clone()),
			SymGraphVal::Symbol(prototype_ptr.clone()),
		);

		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::OverrideOf(prototype_ptr))
			.or_insert(SymGraphVal::Symbols(vec![]));

		let SymGraphVal::Symbols(overrides) = sgn.value_mut() else {
			unreachable!();
		};

		overrides.push(override_ptr);
	}

	/// Returns `Err` if `mixin_ix` has already been expanded into `class_ix`.
	pub(crate) fn make_mixin(&self, class_ptr: SymPtr, mixin_ptr: SymPtr) -> Result<(), ()> {
		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::Mixins(class_ptr.clone()))
			.or_insert(SymGraphVal::Symbols(vec![]));

		let SymGraphVal::Symbols(mixins) = sgn.value_mut() else {
			unreachable!();
		};

		if mixins.contains(&mixin_ptr) {
			return Err(());
		}

		mixins.push(mixin_ptr.clone());

		drop(sgn);

		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::MixinRefs(mixin_ptr))
			.or_insert(SymGraphVal::Symbols(vec![]));

		let SymGraphVal::Symbols(classes) = sgn.value_mut() else {
			unreachable!();
		};

		classes.push(class_ptr);

		Ok(())
	}

	#[must_use]
	pub(crate) fn parent_of(&self, child: SymPtr) -> Option<SymGraphRef> {
		let Some(sgn) = self.sym_graph.get(&SymGraphKey::ParentOf(child)) else {
			return None;
		};

		Some(sgn)
	}

	#[must_use]
	pub(crate) fn _holder_of(&self, member: SymPtr) -> Option<SymGraphRef> {
		let Some(sgn) = self.sym_graph.get(&SymGraphKey::Holder(member)) else {
			return None;
		};

		Some(sgn)
	}

	#[must_use]
	pub(crate) fn make_location(&self, src: &Source, span: TextRange) -> Location {
		Location {
			uri: Url::from_file_path(self.paths.resolve(src.id)).unwrap(),
			range: src.make_range(span),
		}
	}

	/// A symbol's "critical span" is the part of its source that's important to serving diagnostics.
	///
	/// For example, a ZScript function definition's critical span starts at its
	/// first qualifier keyword or return type token and ends at the closing
	/// parenthesis of its parameter list or `const` keyword.
	#[must_use]
	pub(crate) fn diag_location<L: LangExt>(
		&self,
		u_sym: &UserSymbol,
		crit_span: fn(&SyntaxNode<L>) -> TextRange,
	) -> Location {
		let src = self.file_with(u_sym);
		let node = src.node_covering(u_sym.id.span);
		self.make_location(src, crit_span(&node))
	}

	pub(crate) fn raise(&self, builder: DiagBuilder) {
		self.diags
			.entry(self.src.id)
			.or_insert(vec![])
			.push(builder.0)
	}
}

impl std::ops::Deref for FrontendContext<'_> {
	type Target = WorkingWorld;

	fn deref(&self) -> &Self::Target {
		self.inner
	}
}
