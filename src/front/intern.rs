//! [`NameInterner`] and [`NameIx`].

use std::{
	borrow::Borrow,
	hash::{Hash, Hasher},
};

use doomfront::rowan::{cursor::SyntaxToken, GreenToken};
use vtutil::pushvec::PushVec;

use crate::FxDashMap;

/// An index into a [`NameInterner`]. Used for symbol lookup.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameIx(u32);

/// A concurrent interner for [`IName`], allowing [32-bit indices](NameIx)
/// to be used as [map keys](crate::Scope) in place of pointers.
#[derive(Debug, Default)]
pub struct NameInterner {
	array: PushVec<IName>,
	map: FxDashMap<IName, NameIx>,
}

impl NameInterner {
	#[must_use]
	pub fn intern(&self, token: &SyntaxToken) -> NameIx {
		self.add(token.green().to_owned())
	}

	#[must_use]
	pub fn resolve(&self, ix: NameIx) -> &str {
		Borrow::borrow(&self.array[ix.0 as usize])
	}

	#[must_use]
	fn add(&self, green: GreenToken) -> NameIx {
		let iname = IName(green);

		let vac = match self.map.entry(iname.clone()) {
			dashmap::mapref::entry::Entry::Occupied(occ) => return *occ.get(),
			dashmap::mapref::entry::Entry::Vacant(vac) => vac,
		};

		let ix = self.array.push(iname);
		debug_assert!(ix < (u32::MAX as usize));
		let ret = NameIx(ix as u32);
		vac.insert(ret);
		ret
	}
}

/// "Interned name"; a [`GreenToken`] with case-insensitive comparison and hashing.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub(crate) struct IName(pub(crate) GreenToken);

impl PartialEq for IName {
	fn eq(&self, other: &Self) -> bool {
		let self_text: &str = self.borrow();
		let o_text: &str = other.borrow();
		self_text.eq_ignore_ascii_case(o_text)
	}
}

impl Eq for IName {}

impl Hash for IName {
	fn hash<H: Hasher>(&self, state: &mut H) {
		let text: &str = self.borrow();

		for c in text.chars() {
			c.to_ascii_lowercase().hash(state);
		}
	}
}

impl Borrow<GreenToken> for IName {
	fn borrow(&self) -> &GreenToken {
		&self.0
	}
}

impl Borrow<str> for IName {
	fn borrow(&self) -> &str {
		let whole = self.0.text();

		if whole.ends_with(['\'', '"']) {
			// Name literal or string literal
			&whole[1..(whole.len() - 1)]
		} else {
			whole
		}
	}
}

impl std::fmt::Display for IName {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.0.text().fmt(f)
	}
}

#[cfg(test)]
mod test {
	use doomfront::rowan::{cursor::SyntaxNode, GreenNode, SyntaxKind};

	use super::*;

	#[test]
	fn smoke() {
		let interner = NameInterner::default();

		let node = GreenNode::new(
			SyntaxKind(0),
			[
				GreenToken::new(SyntaxKind(1), "lorem").into(),
				GreenToken::new(SyntaxKind(2), "'ipsum'").into(),
			],
		);

		let cursor = SyntaxNode::new_root(node);
		let token0 = cursor.first_token().unwrap();
		let token1 = cursor.last_token().unwrap();

		let ix0 = interner.intern(&token0);
		let ix1 = interner.intern(&token1);

		assert_eq!(interner.resolve(ix0), "lorem");
		assert_eq!(interner.resolve(ix1), "ipsum");
	}
}
