use crossbeam::{atomic::AtomicCell, channel::Sender, utils::Backoff};
use doomfront::{rowan::TextRange, LangExt};
use lsp_types::{Location, OneOf, Url};

use crate::{
	core::{
		DefIx, Definition, FileSpan, Scope, Source, SymGraphKey, SymGraphValue, SymIx, Symbol,
		SymbolId, WorkingWorld,
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
	/// `Ok` contains the index of the newly-declared symbol.
	/// `Err` contains the index of the symbol that would have been overwritten.
	pub(crate) fn declare<L: LangExt>(
		&self,
		outer: &mut Scope,
		ns_name: NsName,
		lang: LangId,
		node: &doomfront::rowan::SyntaxNode<L>,
		crit_span: TextRange,
	) -> Result<SymIx, SymIx> {
		let id = SymbolId(FileSpan {
			file_id: self.src.id,
			span: node.text_range(),
		});

		let sym_ix = match outer.entry(ns_name) {
			im::hashmap::Entry::Vacant(vac) => {
				let ix = self.decls.push(Symbol {
					id,
					syn: L::kind_to_raw(node.kind()),
					lang,
					project: self.project_ix as u8,
					crit_span,
					def: AtomicCell::new(DefIx::PLACEHOLDER),
				});

				let sym_ix = SymIx(ix as i32);
				vac.insert(sym_ix);
				sym_ix
			}
			im::hashmap::Entry::Occupied(occ) => {
				return Err(*occ.get());
			}
		};

		Ok(sym_ix)
	}

	/// A returned `Some` contains the index of the overriden symbol.
	pub(crate) fn decl_override<L: LangExt>(
		&self,
		outer: &mut Scope,
		ns_name: NsName,
		lang: LangId,
		node: &doomfront::rowan::SyntaxNode<L>,
		crit_span: TextRange,
	) -> (SymIx, Option<SymIx>) {
		let id = SymbolId(FileSpan {
			file_id: self.src.id,
			span: node.text_range(),
		});

		let ix = self.decls.push(Symbol {
			id,
			syn: L::kind_to_raw(node.kind()),
			lang,
			project: self.project_ix as u8,
			crit_span,
			def: AtomicCell::new(DefIx::PLACEHOLDER),
		});

		let sym_ix = SymIx(ix as i32);
		(sym_ix, outer.insert(ns_name, sym_ix))
	}

	pub(crate) fn make_member(&self, member: SymIx, holder: SymIx) {
		self.sym_graph
			.insert(SymGraphKey::Holder(member), SymGraphValue::Symbol(holder));

		let mut refmut = self
			.sym_graph
			.entry(SymGraphKey::Members(holder))
			.or_insert(SymGraphValue::Symbols(vec![]));

		let SymGraphValue::Symbols(members) = refmut.value_mut() else {
			unreachable!()
		};

		members.push(member);
	}

	pub(crate) fn make_ref_to(&self, span: TextRange, sym_ix: SymIx) {
		let fspan = FileSpan {
			file_id: self.src.id,
			span,
		};

		self.sym_graph
			.insert(SymGraphKey::Reference(fspan), SymGraphValue::Symbol(sym_ix));

		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::Referred(sym_ix))
			.or_insert(SymGraphValue::References(vec![]));

		let SymGraphValue::References(refs) = sgn.value_mut() else {
			unreachable!();
		};

		refs.push(fspan);
	}

	pub(crate) fn make_child_of(&self, parent_ix: SymIx, child_ix: SymIx) {
		self.sym_graph.insert(
			SymGraphKey::ParentOf(child_ix),
			SymGraphValue::Symbol(parent_ix),
		);

		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::ChildrenOf(parent_ix))
			.or_insert(SymGraphValue::Symbols(vec![]));

		let SymGraphValue::Symbols(children) = sgn.value_mut() else {
			unreachable!();
		};

		children.push(child_ix);
	}

	pub(crate) fn make_override_of(&self, prototype_ix: SymIx, override_ix: SymIx) {
		self.sym_graph.insert(
			SymGraphKey::PrototypeFor(override_ix),
			SymGraphValue::Symbol(prototype_ix),
		);

		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::OverrideOf(prototype_ix))
			.or_insert(SymGraphValue::Symbols(vec![]));

		let SymGraphValue::Symbols(overrides) = sgn.value_mut() else {
			unreachable!();
		};

		overrides.push(override_ix);
	}

	/// Returns `Err` if `mixin_ix` has already been expanded into `class_ix`.
	pub(crate) fn make_mixin(&self, class_ix: SymIx, mixin_ix: SymIx) -> Result<(), ()> {
		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::Mixins(class_ix))
			.or_insert(SymGraphValue::Symbols(vec![]));

		let SymGraphValue::Symbols(mixins) = sgn.value_mut() else {
			unreachable!();
		};

		if mixins.contains(&mixin_ix) {
			return Err(());
		}

		mixins.push(mixin_ix);

		drop(sgn);

		let mut sgn = self
			.sym_graph
			.entry(SymGraphKey::MixinRefs(mixin_ix))
			.or_insert(SymGraphValue::Symbols(vec![]));

		let SymGraphValue::Symbols(classes) = sgn.value_mut() else {
			unreachable!();
		};

		classes.push(class_ix);
		Ok(())
	}

	#[must_use]
	pub(crate) fn get_scope_or_sender(
		&self,
		fspan: FileSpan,
	) -> OneOf<Sender<Scope>, Option<Scope>> {
		match self.scope_work.entry(fspan) {
			dashmap::mapref::entry::Entry::Occupied(occ) => match occ.get().try_recv() {
				Ok(scope) => OneOf::Right(Some(scope)),
				Err(_) => OneOf::Right(self.scopes.get(&fspan).map(|s| s.clone())),
			},
			dashmap::mapref::entry::Entry::Vacant(vac) => {
				let (sender, receiver) = crossbeam::channel::bounded(0);
				vac.insert(receiver);
				OneOf::Left(sender)
			}
		}
	}

	#[must_use]
	pub(crate) fn parent_of(&self, child_ix: SymIx) -> Option<SymIx> {
		let Some(sgn) = self.sym_graph.get(&SymGraphKey::ParentOf(child_ix)) else {
			return None;
		};

		let SymGraphValue::Symbol(proto_ix) = sgn.value() else {
			unreachable!();
		};

		Some(*proto_ix)
	}

	#[must_use]
	pub(crate) fn holder_of(&self, member_ix: SymIx) -> &Symbol {
		let sgn = self.sym_graph.get(&SymGraphKey::Holder(member_ix)).unwrap();

		let SymGraphValue::Symbol(proto_ix) = sgn.value() else {
			unreachable!();
		};

		let OneOf::Left(u_sym) = self.symbol(*proto_ix) else {
			unreachable!()
		};

		u_sym
	}

	/// If `sym` is defined, this is an atomic CEX and a load. No state is changed.
	/// If `sym` is pending a definition, use exponential backoff to wait until
	/// the other thread has finished that definition.
	/// If `sym` is undefined, provide a definition for it.
	#[must_use]
	pub(crate) fn require_sym(
		&'w self,
		sym_ix: SymIx,
		sym: &Symbol,
		callback: fn(&FrontendContext, SymIx, &Symbol) -> DefIx,
	) -> DefIx {
		if sym
			.def
			.compare_exchange(DefIx::PLACEHOLDER, DefIx::PENDING)
			.is_ok()
		{
			let src = self.file_with(sym);

			let new_ctx: FrontendContext = Self {
				project_ix: sym.project as usize,
				src,
				..*self
			};

			let ix = callback(&new_ctx, sym_ix, sym);
			sym.def.store(ix);
			return ix;
		}

		let backoff = Backoff::new();
		let mut status = sym.def.load();

		while status == DefIx::PENDING {
			backoff.snooze();
			status = sym.def.load();
		}

		status
	}

	#[must_use]
	pub(crate) fn get_or_require_def(
		&self,
		ix: SymIx,
		callback: fn(&FrontendContext, SymIx, &Symbol) -> DefIx,
	) -> &Definition {
		match self.symbol(ix) {
			OneOf::Left(u_sym) => {
				let def_ix = self.require_sym(ix, u_sym, callback);
				&self.defs[def_ix.0 as usize]
			}
			OneOf::Right(in_sym) => &in_sym.def,
		}
	}

	#[must_use]
	pub(crate) fn make_location(&self, src: &Source, span: TextRange) -> Location {
		Location {
			uri: Url::from_file_path(self.paths.resolve(src.id)).unwrap(),
			range: src.make_range(span),
		}
	}

	/// Returns `None` if `sym_ix` points to an internal symbol.
	#[must_use]
	pub(crate) fn location_of(&self, sym_ix: SymIx) -> Option<Location> {
		let OneOf::Left(sym) = self.symbol(sym_ix) else {
			return None;
		};

		let src = self.file_with(sym);
		Some(self.make_location(src, sym.crit_span))
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
