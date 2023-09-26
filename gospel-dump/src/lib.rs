/*!

Pretty nice printing of hex dumps.

This is intended to be used alongside `gospel` with the [`dump`] function, but can be used with
plain byte slices with [`Dump::new`].

See [`Dump`] for main documentation.

*/

use std::fmt;

/// A hex dump of a byte slice.
///
/// This can be printed with `{:x}`, `{:X}`, or `{:b}` specifiers, to show the dump in the
/// specified format. In addition, it supports the following format specifiers:
///
/// - `#`: enable ANSI color escape codes
/// - `N`: number of lines to write (defaults to printing until the end)
/// - `.M`: number of bytes per line (defaults to as many as fits inside 240 terminal columns)
#[must_use]
pub struct Dump<'a> {
	start: usize,
	end: usize,
	data: &'a [u8],
	num_width_as: usize,
	#[allow(clippy::type_complexity)]
	preview: Option<Box<dyn Fn(&[u8]) -> String + 'static>>,
}

/// Creates a dump of a [`gospel::read::Reader`].
///
/// Only available when the `gospel` feature is enabled, which it is by default.
#[cfg(feature = "gospel")]
pub fn dump<'a>(f: &gospel::read::Reader<'a>) -> Dump<'a> {
	Dump::new(f.data()).start(f.pos())
}

impl<'a> Dump<'a> {
	/// Creates a dump of a byte slice.
	pub fn new(data: &'a [u8]) -> Self {
		Self {
			start: 0,
			end: data.len(),
			data,
			num_width_as: data.len(),
			preview: Some(Box::new(|a| String::from_utf8_lossy(a).into_owned()))
		}
	}

	/// Sets the starting point of the dump.
	pub fn start(self, start: usize) -> Self {
		Self {
			start,
			..self
		}
	}

	/// Sets the end point of the dump.
	///
	/// Options passed when printing will have priority.
	pub fn end(self, end: usize) -> Self {
		Self {
			end,
			..self
		}
	}

	/// Sets the end point of the dump, relative to the starting point.
	///
	/// Options passed when printing will have priority.
	pub fn len(self, len: usize) -> Self {
		Self {
			end: self.start + len,
			..self
		}
	}

	/// Sets the number of digits to display for the byte offset to fit the given number.
	pub fn num_width_as(self, num_width_as: usize) -> Self {
		Self {
			num_width_as,
			..self
		}
	}

	/// Set a function to use for displaying the unicode preview.
	///
	/// By default, this uses `String::from_utf8_lossy()`.
	pub fn preview(self, preview: impl Fn(&[u8]) -> String + 'static) -> Self {
		Self {
			preview: Some(Box::new(preview)),
			..self
		}
	}

	/// Removes the unicode preview.
	pub fn no_preview(self) -> Self {
		Self {
			preview: None,
			..self
		}
	}
}

impl fmt::UpperHex for Dump<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.print(f, |x, f| write!(f, "{x:02X}"), 2)
	}
}

impl fmt::LowerHex for Dump<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.print(f, |x, f| write!(f, "{x:02x}"), 2)
	}
}

impl fmt::Binary for Dump<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.print(f, |x, f| write!(f, "{x:08b}"), 8)
	}
}

impl Dump<'_> {
	fn print(
		&self,
		f: &mut fmt::Formatter,
		write: impl Fn(u8, &mut fmt::Formatter) -> fmt::Result,
		cell_width: usize,
	) -> fmt::Result {
		const SCREEN_WIDTH: usize = 240;

		let num_width = if self.num_width_as == 0 {
			0
		} else {
			format!("{:X}", self.num_width_as).len()
		};

		let has_text = !f.sign_minus();
		let lines = f.width().unwrap_or(usize::MAX);
		let width = f.precision().unwrap_or_else(|| {
			let c = cell_width + 1 + usize::from(has_text);
			let w = num_width + usize::from(num_width != 0) + usize::from(has_text);
			(SCREEN_WIDTH - w) / c / 4 * 4
		}).max(1);

		if self.data[self.start..self.end].is_empty() || lines == 0 {
			let pos = self.start;
			if num_width > 0 {
				let s = format!("{:X}", pos);
				if s.len() < num_width {
					sgr(f, "2;33")?;
					for _ in s.len()..num_width {
						f.write_str("0")?;
					}
				}
				sgr(f, "33")?;
				f.write_str(&s)?;
				sgr(f, "")?;
				f.write_str(" ")?;
			}
			f.write_str("\n")?;
		}

		for (i, chunk) in self.data[self.start..self.end].chunks(width).take(lines).enumerate() {
			let pos = self.start + i * width;

			if num_width > 0 {
				let s = format!("{:X}", pos);
				if s.len() < num_width {
					sgr(f, "2;33")?;
					for _ in s.len()..num_width {
						f.write_str("0")?;
					}
				}
				sgr(f, "33")?;
				f.write_str(&s)?;
				sgr(f, "")?;
				f.write_str(" ")?;
			}

			let mut prev_color = "";
			for (i, &b) in chunk.iter().enumerate() {
				if i != 0 {
					f.write_str(" ")?;
				}

				let color = match b {
					0x00        => "2",
					0xFF        => "38;5;9",
					0x20..=0x7E => "38;5;10",
					_           => "",
				};

				if prev_color != color {
					sgr(f, color)?;
					prev_color = color;
				}
				write(b, f)?;
			}
			sgr(f, "")?;

			if let Some(preview) = &self.preview {
				for _ in chunk.len()..width {
					f.write_str("   ")?;
				}
				f.write_str(" ▏")?;

				for char in preview(chunk).chars() {
					let (color, char) = match char {
						'�' => ("2", '·'),
						c if c.is_control() => ("38;5;8", '·'),
						c => ("", c),
					};
					if prev_color != color {
						sgr(f, color)?;
						prev_color = color;
					}
					write!(f, "{char}")?;
				}

				sgr(f, "")?;
			}
			f.write_str("\n")?;
		}

		Ok(())
	}
}

fn sgr(f: &mut fmt::Formatter, arg: &str) -> fmt::Result {
	if f.alternate() {
		write!(f, "\x1B[0;{arg}m")
	} else {
		Ok(())
	}
}
