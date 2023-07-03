//! Functions for analyzing newlines and wide characters.
//!
//! All code in this module can be assumed to be from rust-analyzer unless
//! explicitly stated to be otherwise.

use doomfront::rowan::TextSize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct WideChar {
	/// Start offset of a character inside a line, zero-based.
	start: TextSize,
	/// End offset of a character inside a line, zero-based.
	end: TextSize,
}

#[must_use]
pub(crate) fn compute_newlines(src: &str) -> Box<[TextSize]> {
	let mut ret = vec![];
	let _ = analyze_source_file_generic(src, src.len(), TextSize::from(0), &mut ret);
	ret.into_boxed_slice()
}

/// `scan_len` determines the number of bytes in `src` to scan. Note that the
/// function can read past `scan_len` if a multi-byte character start within the
/// range but extends past it. The overflow is returned by the function.
fn analyze_source_file_generic(
	src: &str,
	scan_len: usize,
	output_offset: TextSize,
	lines: &mut Vec<TextSize>,
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
				let _ = WideChar {
					start: pos,
					end: pos + TextSize::from(char_len as u32),
				};
				// TODO: Store this in some kind of map.
			}
		}

		i += char_len;
	}

	i - scan_len
}
