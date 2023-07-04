//! Functions and types for dealing in strings in terms of lines.
//!
//! All code in this module can be assumed to be from rust-analyzer unless
//! explicitly stated to be otherwise.

#![allow(unused)]

use doomfront::rowan::{TextRange, TextSize};
use lsp_types::TextDocumentContentChangeEvent;
use nohash_hasher::IntMap;

/// `(line, column)` information in the native, UTF-8 encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct LineCol {
	/// Zero-based.
	pub(crate) line: u32,
	/// Zero-based UTF-8 offset.
	pub(crate) col: u32,
}

/// A kind of wide character encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub(crate) enum WideEncoding {
	/// UTF-16.
	Utf16,
	/// UTF-32.
	Utf32,
}

impl WideEncoding {
	/// Returns the number of code units it takes to encode `text` in this encoding.
	pub(crate) fn measure(&self, text: &str) -> usize {
		match self {
			WideEncoding::Utf16 => text.encode_utf16().count(),
			WideEncoding::Utf32 => text.chars().count(),
		}
	}
}

/// `(line, column)` information in wide encodings.
///
/// See [`WideEncoding`] for the kinds of wide encodings available.
//
// Deliberately not a generic type and different from `LineCol`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct WideLineCol {
	/// Zero-based.
	pub(crate) line: u32,
	/// Zero-based.
	pub(crate) col: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct WideChar {
	/// Start offset of a character inside a line, zero-based.
	start: TextSize,
	/// End offset of a character inside a line, zero-based.
	end: TextSize,
}

impl WideChar {
	/// Returns the length in 8-bit UTF-8 code units.
	fn len(&self) -> TextSize {
		self.end - self.start
	}

	/// Returns the length in UTF-16 or UTF-32 code units.
	fn wide_len(&self, enc: WideEncoding) -> u32 {
		match enc {
			WideEncoding::Utf16 => {
				if self.len() == TextSize::from(4) {
					2
				} else {
					1
				}
			}
			WideEncoding::Utf32 => 1,
		}
	}
}

/// Maps flat [`TextSize`] offsets to/from `(line, column)` representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LineIndex {
	/// Offset the beginning of each line (except the first, which always has offset 0).
	newlines: Box<[TextSize]>,
	/// List of non-ASCII characters on each line.
	line_wide_chars: IntMap<u32, Box<[WideChar]>>,
	/// The length of the entire text.
	len: TextSize,
}

impl LineIndex {
	/// Returns a `LineIndex` for the `text`.
	pub(crate) fn new(text: &str) -> LineIndex {
		let (newlines, line_wide_chars) = analyze_source_file(text);

		LineIndex {
			newlines: newlines.into_boxed_slice(),
			line_wide_chars,
			len: TextSize::of(text),
		}
	}

	/// Transforms the `TextSize` into a `LineCol`.
	///
	/// # Panics
	///
	/// If the offset is invalid. See [`Self::try_line_col`].
	pub(crate) fn line_col(&self, offset: TextSize) -> LineCol {
		self.try_line_col(offset).expect("invalid offset")
	}

	/// Transforms the `TextSize` into a `LineCol`.
	///
	/// Returns `None` if the `offset` was invalid, e.g. if it extends past the end of the text or
	/// points to the middle of a multi-byte character.
	pub(crate) fn try_line_col(&self, offset: TextSize) -> Option<LineCol> {
		if offset > self.len {
			return None;
		}
		let line = self.newlines.partition_point(|&it| it <= offset);
		let start = self.start_offset(line)?;
		let col = offset - start;
		let ret = LineCol {
			line: line as u32,
			col: col.into(),
		};
		self.line_wide_chars
			.get(&ret.line)
			.into_iter()
			.flat_map(|it| it.iter())
			.all(|it| col <= it.start || it.end <= col)
			.then_some(ret)
	}

	/// Transforms the `LineCol` into a `TextSize`.
	pub(crate) fn offset(&self, line_col: LineCol) -> Option<TextSize> {
		self.start_offset(line_col.line as usize)
			.map(|start| start + TextSize::from(line_col.col))
	}

	#[must_use]
	pub(crate) fn get_line(&self, line: usize) -> Option<TextSize> {
		self.newlines.get(line).copied()
	}

	fn start_offset(&self, line: usize) -> Option<TextSize> {
		match line.checked_sub(1) {
			None => Some(TextSize::from(0)),
			Some(it) => self.newlines.get(it).copied(),
		}
	}

	/// Transforms the `LineCol` with the given `WideEncoding` into a `WideLineCol`.
	pub(crate) fn to_wide(&self, enc: WideEncoding, line_col: LineCol) -> Option<WideLineCol> {
		let mut col = line_col.col;
		if let Some(wide_chars) = self.line_wide_chars.get(&line_col.line) {
			for c in wide_chars.iter() {
				if u32::from(c.end) <= line_col.col {
					col = col.checked_sub(u32::from(c.len()) - c.wide_len(enc))?;
				} else {
					// From here on, all utf16 characters come *after* the character we are mapping,
					// so we don't need to take them into account
					break;
				}
			}
		}
		Some(WideLineCol {
			line: line_col.line,
			col,
		})
	}

	/// Transforms the `WideLineCol` with the given `WideEncoding` into a `LineCol`.
	pub(crate) fn to_utf8(&self, enc: WideEncoding, line_col: WideLineCol) -> Option<LineCol> {
		let mut col = line_col.col;
		if let Some(wide_chars) = self.line_wide_chars.get(&line_col.line) {
			for c in wide_chars.iter() {
				if col > u32::from(c.start) {
					col = col.checked_add(u32::from(c.len()) - c.wide_len(enc))?;
				} else {
					// From here on, all utf16 characters come *after* the character we are mapping,
					// so we don't need to take them into account
					break;
				}
			}
		}
		Some(LineCol {
			line: line_col.line,
			col,
		})
	}

	/// Given a range [start, end), returns a sorted iterator of non-empty ranges [start, x1), [x1,
	/// x2), ..., [xn, end) where all the xi, which are positions of newlines, are inside the range
	/// [start, end).
	pub(crate) fn lines(&self, range: TextRange) -> impl Iterator<Item = TextRange> + '_ {
		let lo = self.newlines.partition_point(|&it| it < range.start());
		let hi = self.newlines.partition_point(|&it| it <= range.end());
		let all = std::iter::once(range.start())
			.chain(self.newlines[lo..hi].iter().copied())
			.chain(std::iter::once(range.end()));

		all.clone()
			.zip(all.skip(1))
			.map(|(lo, hi)| TextRange::new(lo, hi))
			.filter(|it| !it.is_empty())
	}

	/// Returns the length of the original text.
	pub(crate) fn len(&self) -> TextSize {
		self.len
	}
}

/// This is adapted from the rustc_span crate, https://github.com/rust-lang/rust/blob/master/compiler/rustc_span/src/analyze_source_file.rs
fn analyze_source_file(src: &str) -> (Vec<TextSize>, IntMap<u32, Box<[WideChar]>>) {
	assert!(src.len() < !0u32 as usize);
	let mut lines = vec![];
	let mut line_wide_chars = IntMap::<u32, Vec<WideChar>>::default();

	// Calls the right implementation, depending on hardware support available.
	analyze_source_file_dispatch(src, &mut lines, &mut line_wide_chars);

	(
		lines,
		line_wide_chars
			.into_iter()
			.map(|(k, v)| (k, v.into_boxed_slice()))
			.collect(),
	)
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
fn analyze_source_file_dispatch(
	src: &str,
	lines: &mut Vec<TextSize>,
	multi_byte_chars: &mut IntMap<u32, Vec<WideChar>>,
) {
	if is_x86_feature_detected!("sse2") {
		// SAFETY: SSE2 support was checked
		unsafe {
			analyze_source_file_sse2(src, lines, multi_byte_chars);
		}
	} else {
		analyze_source_file_generic(src, src.len(), TextSize::from(0), lines, multi_byte_chars);
	}
}

/// Checks 16 byte chunks of text at a time. If the chunk contains
/// something other than printable ASCII characters and newlines, the
/// function falls back to the generic implementation. Otherwise it uses
/// SSE2 intrinsics to quickly find all newlines.
#[target_feature(enable = "sse2")]
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe fn analyze_source_file_sse2(
	src: &str,
	lines: &mut Vec<TextSize>,
	multi_byte_chars: &mut IntMap<u32, Vec<WideChar>>,
) {
	#[cfg(target_arch = "x86")]
	use std::arch::x86::*;
	#[cfg(target_arch = "x86_64")]
	use std::arch::x86_64::*;

	const CHUNK_SIZE: usize = 16;

	let src_bytes = src.as_bytes();

	let chunk_count = src.len() / CHUNK_SIZE;

	// This variable keeps track of where we should start decoding a
	// chunk. If a multi-byte character spans across chunk boundaries,
	// we need to skip that part in the next chunk because we already
	// handled it.
	let mut intra_chunk_offset = 0;

	for chunk_index in 0..chunk_count {
		let ptr = src_bytes.as_ptr() as *const __m128i;
		// We don't know if the pointer is aligned to 16 bytes, so we
		// use `loadu`, which supports unaligned loading.
		let chunk = _mm_loadu_si128(ptr.add(chunk_index));

		// For character in the chunk, see if its byte value is < 0, which
		// indicates that it's part of a UTF-8 char.
		let multibyte_test = _mm_cmplt_epi8(chunk, _mm_set1_epi8(0));
		// Create a bit mask from the comparison results.
		let multibyte_mask = _mm_movemask_epi8(multibyte_test);

		// If the bit mask is all zero, we only have ASCII chars here:
		if multibyte_mask == 0 {
			assert!(intra_chunk_offset == 0);

			// Check for newlines in the chunk
			let newlines_test = _mm_cmpeq_epi8(chunk, _mm_set1_epi8(b'\n' as i8));
			let newlines_mask = _mm_movemask_epi8(newlines_test);

			if newlines_mask != 0 {
				// All control characters are newlines, record them
				let mut newlines_mask = 0xFFFF0000 | newlines_mask as u32;
				let output_offset = TextSize::from((chunk_index * CHUNK_SIZE + 1) as u32);

				loop {
					let index = newlines_mask.trailing_zeros();

					if index >= CHUNK_SIZE as u32 {
						// We have arrived at the end of the chunk.
						break;
					}

					lines.push(TextSize::from(index) + output_offset);

					// Clear the bit, so we can find the next one.
					newlines_mask &= (!1) << index;
				}
			}
			continue;
		}

		// The slow path.
		// There are control chars in here, fallback to generic decoding.
		let scan_start = chunk_index * CHUNK_SIZE + intra_chunk_offset;
		intra_chunk_offset = analyze_source_file_generic(
			&src[scan_start..],
			CHUNK_SIZE - intra_chunk_offset,
			TextSize::from(scan_start as u32),
			lines,
			multi_byte_chars,
		);
	}

	// There might still be a tail left to analyze
	let tail_start = chunk_count * CHUNK_SIZE + intra_chunk_offset;
	if tail_start < src.len() {
		analyze_source_file_generic(
			&src[tail_start..],
			src.len() - tail_start,
			TextSize::from(tail_start as u32),
			lines,
			multi_byte_chars,
		);
	}
}

#[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
// The target (or compiler version) does not support SSE2 ...
fn analyze_source_file_dispatch(
	src: &str,
	lines: &mut Vec<TextSize>,
	multi_byte_chars: &mut IntMap<u32, Vec<WideChar>>,
) {
	analyze_source_file_generic(src, src.len(), TextSize::from(0), lines, multi_byte_chars);
}

// `scan_len` determines the number of bytes in `src` to scan. Note that the
// function can read past `scan_len` if a multi-byte character start within the
// range but extends past it. The overflow is returned by the function.
fn analyze_source_file_generic(
	src: &str,
	scan_len: usize,
	output_offset: TextSize,
	lines: &mut Vec<TextSize>,
	multi_byte_chars: &mut IntMap<u32, Vec<WideChar>>,
) -> usize {
	assert!(src.len() >= scan_len);
	let mut i = 0;
	let src_bytes = src.as_bytes();

	while i < scan_len {
		let byte = unsafe {
			// We verified that i < scan_len <= src.len()
			*src_bytes.get_unchecked(i)
		};

		// How much to advance in order to get to the next UTF-8 char in the
		// string.
		let mut char_len = 1;

		if byte == b'\n' {
			lines.push(TextSize::from(i as u32 + 1) + output_offset);
		} else if byte >= 127 {
			// The slow path: Just decode to `char`.
			let c = src[i..].chars().next().unwrap();
			char_len = c.len_utf8();

			let pos = TextSize::from(i as u32) + output_offset;

			if char_len > 1 {
				assert!((2..=4).contains(&char_len));
				let mbc = WideChar {
					start: pos,
					end: pos + TextSize::from(char_len as u32),
				};
				multi_byte_chars
					.entry(lines.len() as u32)
					.or_default()
					.push(mbc);
			}
		}

		i += char_len;
	}

	i - scan_len
}

/// From rust-analyzer.
pub(crate) fn splice_changes(text: &mut String, mut changes: Vec<TextDocumentContentChangeEvent>) {
	// Skip to the last full document change,
	// as it invalidates all previous changes anyways.
	let mut start = changes
		.iter()
		.rev()
		.position(|change| change.range.is_none())
		.map_or(0, |idx| changes.len() - idx - 1);

	match changes.get_mut(start) {
		// Peek at the first content change as an optimization.
		Some(lsp_types::TextDocumentContentChangeEvent {
			range: None, text, ..
		}) => {
			*text = std::mem::take(text);
			start += 1;

			// The only change is a full document update.
			if start == changes.len() {
				return;
			}
		}
		Some(_) => {}
		// We received no content changes.
		None => return,
	}

	let mut pos_db = PositionDb::new(text);

	// The changes we got must be applied sequentially, but can cross lines so we
	// have to keep our line index updated. Some clients (e.g. VSCode) sort the
	// ranges in reverse. As an optimization, we remember the last valid line in
	// the index and only rebuild it if needed.
	let mut index_valid = u32::MAX;

	for change in changes {
		// The `None` case can't happen as we have handled it above already.
		if let Some(range) = change.range {
			if index_valid <= range.end.line {
				pos_db = PositionDb::new(text);
			}

			index_valid = range.start.line;

			if let Some(range) = pos_db.text_range_utf8(analysis_range(range)) {
				text.replace_range(std::ops::Range::<usize>::from(range), &change.text);
			}
		}
	}
}

/// Converts between flat [`TextSize`] offsets and `(line, col)`
/// representation, with handling for both UTF-8 and UTF-16 encoded text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PositionDb {
	inner: LineIndex,
}

impl PositionDb {
	/// Returns a new `PositionDb` for the text.
	#[must_use]
	pub(crate) fn new(text: &str) -> Self {
		Self {
			inner: LineIndex::new(text),
		}
	}

	/// Returns the `PositionUtf16` for this `TextSize`, or `None` if it is out of bounds.
	///
	/// # Panics
	///
	/// Upon internal error.
	#[must_use]
	pub(crate) fn position_utf16(&self, text_size: TextSize) -> Option<PositionUtf16> {
		self.position_to_utf16(self.position_utf8(text_size)?)
	}

	/// Returns the `TextSize` for this `PositionUtf16`, or `None` if it is out of bounds.
	///
	/// # Panics
	///
	/// Upon internal error.
	#[must_use]
	pub(crate) fn text_size_utf16(&self, pos: PositionUtf16) -> Option<TextSize> {
		self.text_size_utf8(self.position_to_utf8(pos)?)
	}

	/// Returns the `RangeUtf16` for this `TextRange`, or `None` if it is out of bounds.
	///
	/// # Panics
	///
	/// Upon internal error.
	#[must_use]
	pub(crate) fn range_utf16(&self, text_range: TextRange) -> Option<RangeUtf16> {
		self.range_to_utf16(self.range_utf8(text_range)?)
	}

	/// Returns the `TextRange` for this `RangeUtf16`, or `None` if it is out of bounds.
	///
	/// # Panics
	///
	/// Upon internal error.
	#[must_use]
	pub(crate) fn text_range_utf16(&self, range: RangeUtf16) -> Option<TextRange> {
		self.text_range_utf8(self.range_to_utf8(range)?)
	}

	fn position_utf8(&self, text_size: TextSize) -> Option<PositionUtf8> {
		let lc = self.inner.try_line_col(text_size)?;
		Some(PositionUtf8 {
			line: lc.line,
			col: lc.col,
		})
	}

	fn text_size_utf8(&self, pos: PositionUtf8) -> Option<TextSize> {
		let lc = LineCol {
			line: pos.line,
			col: pos.col,
		};

		self.inner.offset(lc)
	}

	fn range_utf8(&self, text_range: TextRange) -> Option<RangeUtf8> {
		Some(RangeUtf8 {
			start: self.position_utf8(text_range.start())?,
			end: self.position_utf8(text_range.end())?,
		})
	}

	fn text_range_utf8(&self, range: RangeUtf8) -> Option<TextRange> {
		Some(TextRange::new(
			self.text_size_utf8(range.start)?,
			self.text_size_utf8(range.end)?,
		))
	}

	fn position_to_utf16(&self, pos: PositionUtf8) -> Option<PositionUtf16> {
		let lc = LineCol {
			line: pos.line,
			col: pos.col,
		};

		let wide = self.inner.to_wide(WideEncoding::Utf16, lc)?;

		Some(PositionUtf16 {
			line: wide.line,
			col: wide.col,
		})
	}

	fn position_to_utf8(&self, pos: PositionUtf16) -> Option<PositionUtf8> {
		let wide = WideLineCol {
			line: pos.line,
			col: pos.col,
		};

		let lc = self.inner.to_utf8(WideEncoding::Utf16, wide)?;

		Some(PositionUtf8 {
			line: lc.line,
			col: lc.col,
		})
	}

	fn range_to_utf16(&self, range: RangeUtf8) -> Option<RangeUtf16> {
		Some(RangeUtf16 {
			start: self.position_to_utf16(range.start)?,
			end: self.position_to_utf16(range.end)?,
		})
	}

	fn range_to_utf8(&self, range: RangeUtf16) -> Option<RangeUtf8> {
		Some(RangeUtf8 {
			start: self.position_to_utf8(range.start)?,
			end: self.position_to_utf8(range.end)?,
		})
	}

	/// Returns an iterator over the lines in the range.
	pub(crate) fn lines(&self, range: TextRange) -> impl Iterator<Item = TextRange> + '_ {
		self.inner.lines(range)
	}

	/// Returns the length of the original text.
	#[must_use]
	pub(crate) fn len(&self) -> TextSize {
		self.inner.len()
	}

	/// Returns the end position of the original input.
	///
	/// # Panics
	///
	/// Upon internal error.
	#[must_use]
	pub(crate) fn end_position_utf16(&self) -> PositionUtf16 {
		let pos = self.position_utf8(self.len()).expect("len is in range");
		self.position_to_utf16(pos).expect("len can be utf-16")
	}
}

/// A pair of `(line, col)` for UTF-8.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct PositionUtf8 {
	/// Zero-based.
	line: u32,
	/// Zero-based utf8 offset.
	col: u32,
}

/// A pair of start and end positions for UTF-8.
///
/// `start` comes before `end`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct RangeUtf8 {
	/// The start.
	start: PositionUtf8,
	/// The end.
	end: PositionUtf8,
}

/// A pair of `(line, col)` for UTF-16.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct PositionUtf16 {
	/// Zero-based.
	pub(crate) line: u32,
	/// Zero-based.
	pub(crate) col: u32,
}

impl std::fmt::Display for PositionUtf16 {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}", self.line + 1, self.col + 1)
	}
}

/// A pair of start and end positions for UTF-16.
///
/// `start` comes before `end`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct RangeUtf16 {
	/// The start.
	pub(crate) start: PositionUtf16,
	/// The end.
	pub(crate) end: PositionUtf16,
}

impl std::fmt::Display for RangeUtf16 {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}-{}", self.start, self.end)
	}
}

pub(crate) fn analysis_range(range: lsp_types::Range) -> RangeUtf8 {
	RangeUtf8 {
		start: analysis_position(range.start),
		end: analysis_position(range.end),
	}
}

fn analysis_position(pos: lsp_types::Position) -> PositionUtf8 {
	PositionUtf8 {
		line: pos.line,
		col: pos.character,
	}
}
