/*!

Utilities for reading binary data from slices.

Supports both little and big endian, but not runtime choice. See [`Reader`] for more information.

In the common case that you only care about one endianness, the recommended usage is to import
[`Le`] or [`Be`]:
```rust
use gospel::read::{Reader, Le as _};
```

The `as _` ensures that it does not conflict with the similarly-named [`write::Le`](`crate::write::Le`).

*/

use std::ffi::CStr;

/// An error occurred when reading.
#[derive(Debug, thiserror::Error)]
pub enum Error {
	/// Attempted to read mode data than is available.
	#[error("out-of-bounds read of {pos:#X}+{len} (size {size:#X})")]
	Read { pos: usize, len: usize, size: usize },

	/// Attempted to [`seek`](`Reader::seek`) to an out-of-bounds position.
	#[error("out-of-bounds seek to {pos:#X} (size {size:#X})")]
	Seek { pos: usize, size: usize },

	/// Some other error happened.
	///
	/// This is normally used by [`check`](`Reader::check`), but can be used to embed any error you want.
	#[error("error at {pos:#X}: {source}")]
	Other { pos: usize, #[source] source: Box<dyn std::error::Error + Send + Sync> },
}

/// Type alias for `Result<T, Error>`.
pub type Result<T, E=Error> = std::result::Result<T, E>;

impl Error {
	/// The position the error happened at.
	pub fn pos(&self) -> usize {
		match self {
			Error::Seek { pos, .. } => *pos,
			Error::Read { pos, .. } => *pos,
			Error::Other { pos, .. } => *pos,
		}
	}

	/// The position the error happened at, but mutably.
	///
	/// This can be useful for mapping errors in subslices.
	pub fn pos_mut(&mut self) -> &mut usize {
		match self {
			Error::Seek { pos, .. } => pos,
			Error::Read { pos, .. } => pos,
			Error::Other { pos, .. } => pos,
		}
	}
}

/// A [`check_T`](`Reader::check_T`) failed.
#[derive(Clone, Debug, thiserror::Error)]
#[error("mismatched {type_}. expected: {expected}, got: {got}", type_ = std::any::type_name::<T>())]
pub struct CheckError<T: std::fmt::Display> {
	pub expected: T,
	pub got: T,
}

/// A [`check`](`Reader::check`) failed.
///
/// This is different type from [`CheckError`] in order to display the message in a nicer way.
#[derive(Clone, Debug, thiserror::Error)]
pub struct CheckBytesError {
	pub expected: Vec<u8>,
	pub got: Vec<u8>,
}

impl std::fmt::Display for CheckBytesError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let mut got = Vec::new();
		let mut exp = Vec::new();
		for (&g, &e) in std::iter::zip(&self.got, &self.expected) {
			got.extend(std::ascii::escape_default(g).map(char::from));
			exp.extend(std::ascii::escape_default(e).map(char::from));
			while got.len() < exp.len() { got.push('░') }
			while exp.len() < got.len() { exp.push('░') }
		}
		writeln!(f, "mismatched bytes.")?;
		writeln!(f, "expected: b\"{}\"", String::from_iter(exp))?;
		write  !(f, "got:      b\"{}\"", String::from_iter(got))
	}
}

#[cfg(doc)]
#[doc(hidden)]
pub type T = ();

/// An incremental reader from a byte slice.
///
/// The main functions of interest are [`u32_le`](`Self::T`) and the like, which read the relevant
/// primitives from the slice. In these docs, they are abbreviated to a single function for
/// simplicity.
///
/// Supported primitives are `u8..=u128`, `i8..=i128`, `f32`, `f64`.
///
/// The functions are suffixed with either `_le` or `_be`, for endianness. To use unsuffixed
/// versions, import either the [`Le`] or [`Be`] trait.
///
/// Cloning a `Reader` is cheap, but it does not implement [`Copy`] for similar reasons as
/// [`Range`](`std::ops::Range`).
#[derive(Clone)]
pub struct Reader<'a> {
	pos: usize,
	data: &'a [u8],
}

impl<'a> std::fmt::Debug for Reader<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Reader")
			.field("pos", &self.pos)
			.field("data", &format_args!("[_; {}]", self.data.len()))
			.finish()
	}
}

impl<'a> Reader<'a> {
	/// Constructs a new `Reader`.
	#[inline(always)]
	pub fn new(data: &'a [u8]) -> Reader<'a> {
		Self {
			pos: 0,
			data,
		}
	}

	/// Returns the read position of the reader.
	///
	/// To change the position, use [`seek`](`Self::seek`) or [`at`](`Self::at`).
	#[must_use]
	#[inline(always)]
	pub fn pos(&self) -> usize {
		self.pos
	}

	/// Returns the total length of the input.
	///
	/// Note that this is the total length, which does not change. See
	/// [`remaining`](`Self::remaining`) for the number of bytes left that can be read.
	#[must_use]
	#[inline(always)]
	pub fn len(&self) -> usize {
		self.data.len()
	}

	/// Returns true if there are no more bytes left to read.
	///
	/// Note that unlike slices, this is based on [`remaining`](`Self::remaining`), not
	/// [`len`](`Self::len`).
	#[must_use]
	#[inline(always)]
	pub fn is_empty(&self) -> bool {
		self.remaining().is_empty()
	}

	/// Returns the remaining data in the buffer.
	#[must_use]
	#[inline(always)]
	pub fn remaining(&self) -> &'a [u8] {
		&self.data[self.pos()..]
	}

	/// Returns the data being read from.
	///
	/// This is the full slice; for only the remainder, see [`remaining`](`Self::remaining`).
	#[must_use]
	#[inline(always)]
	pub fn data(&self) -> &'a [u8] {
		self.data
	}

	#[cfg(doc)]
	/// Read a primitive from the input.
	pub fn T(&mut self) -> Result<T> {
		Ok(T::from_bytes(self.array()?))
	}

	/// Reads a slice of data from the input. No copying is done.
	///
	/// Returns an error if there is not enough data left, in which case the read position is
	/// unchanged.
	#[inline(always)]
	pub fn slice(&mut self, len: usize) -> Result<&'a [u8]> {
		if len > self.remaining().len() {
			return Err(Error::Read { pos: self.pos(), len, size: self.len() });
		}
		let pos = self.pos;
		self.pos += len;
		Ok(&self.data[pos..pos+len])
	}

	/// Reads a fixed-size slice of data from the input.
	///
	/// Handles errors identically to [`slice`](`Self::slice`).
	#[inline(always)]
	pub fn array<const N: usize>(&mut self) -> Result<[u8; N]> {
		let mut x = [0; N];
		self.read_into(&mut x)?;
		Ok(x)
	}

	/// Reads a slice of data into a preexisting buffer.
	///
	/// Handles errors identically to [`slice`](`Self::slice`).
	#[inline(always)]
	pub fn read_into(&mut self, buf: &mut [u8]) -> Result<()> {
		buf.copy_from_slice(self.slice(buf.len())?);
		Ok(())
	}

	/// Reads data to and including the next null byte.
	#[inline(always)]
	pub fn cstr(&mut self) -> Result<&CStr> {
		match CStr::from_bytes_until_nul(self.remaining()) {
			Ok(cs) => {
				self.pos += cs.to_bytes_with_nul().len();
				Ok(cs)
			}
			Err(err) => {
				Err(Error::Other { pos: self.pos(), source: Box::new(err) })
			}
		}
	}


	/// Sets the read position.
	///
	/// Returns an error if the position is out of bounds.
	///
	/// See [`at`](`Self::at`) for a version that returns a copy.
	#[inline(always)]
	pub fn seek(&mut self, pos: usize) -> Result<()> {
		if pos > self.len() {
			return Err(Error::Seek { pos, size: self.len() })
		}
		self.pos = pos;
		Ok(())
	}

	/// Returns a copy of the reader at the specified position.
	///
	/// See also [`ptrN`](`Self::ptrN`) for a shorthand for the common pattern of
	/// `f.clone().at(f.u32()? as usize)?`.
	#[inline(always)]
	pub fn at(&self, pos: usize) -> Result<Self> {
		let mut a = self.clone();
		a.seek(pos)?;
		Ok(a)
	}

	/// Reads a number of bytes and returns an error if they are not as expected.
	///
	/// If it does not match, the read position is not affected.
	#[inline(always)]
	pub fn check(&mut self, v: &[u8]) -> Result<()> {
		let pos = self.pos();
		let u = self.slice(v.len())?;
		if u != v {
			self.pos = pos;
			return Err(Error::Other { pos, source: CheckBytesError {
				got:      u.to_owned(),
				expected: v.to_owned(),
			}.into() })
		}
		Ok(())
	}

	#[cfg(doc)]
	/// Read a primitive from the input, giving an error if it is not as expected.
	///
	/// If it not match, the read position is not affected.
	pub fn check_T(&mut self, v: T) -> Result<()> {
		let pos = self.pos();
		let u = self.T()?;
		if u != v {
			self.pos = pos;
			return Err(Error::Other { pos, source: CheckError {
				got: u,
				expected: v,
			}.into() })
		}
		Ok(())
	}

	#[cfg(doc)]
	/// Read a `uN` primitive from the input, and return a new `Reader` at that position.
	///
	/// This is a shorthand for the common pattern of `f.clone().at(f.uN()? as usize)?`.
	///
	/// Note that no checking is made that the value actually fits inside a `usize`;
	/// the higher bits are simply discarded. This is primarily relevant with `ptr128`,
	/// and requires humongous amounts of memory to exhibit issues.
	pub fn ptrN(&mut self) -> Result<Self> {
		self.clone().at(self.uN()? as usize)
	}

	/// Rounds the read position up to the next multiple of `size`, returning the skipped data.
	#[inline(always)]
	pub fn align(&mut self, size: usize) -> Result<&'a [u8]> {
		self.slice((size-(self.pos()%size))%size)
	}

	/// Rounds the read position up to the next multiple of `size`, returning an error if the skipped bytes are nonzero.
	#[inline(always)]
	pub fn align_zeroed(&mut self, size: usize) -> Result<()> {
		let pos = self.pos();
		let u = self.align(size)?;
		if u.iter().any(|a| *a != 0) {
			self.pos = pos;
			let v = vec![0; size];
			return Err(Error::Other { pos, source: CheckBytesError {
				got:      u.to_owned(),
				expected: v.to_owned(),
			}.into() })
		}
		Ok(())
	}

	/// Creates a hexdump of the data at the current position.
	///
	/// Only available with the `dump` feature is enabled.
	#[cfg(feature = "dump")]
	pub fn dump(&self) -> crate::dump::Dump<'a> {
		crate::dump::Dump::new(self.data()).start(self.pos())
	}
}

mod seal { pub trait Sealed: Sized {} }
impl seal::Sealed for Reader<'_> {}

macro_rules! primitives {
	(
		$(#[$trait_attrs:meta])* trait $trait:ident;
		$suf:ident, $conv:ident;
		{ $($type:ident),* }
		{ $($ptr:tt),* }
	) => { paste::paste! {
		#[doc(hidden)]
		impl<'a> Reader<'a> {
			$(#[inline(always)] pub fn [<$type $suf>](&mut self) -> Result<$type> {
				Ok($type::$conv(self.array()?))
			})*
			$(#[inline(always)] pub fn [<check_ $type $suf>](&mut self, v: $type) -> Result<()> {
				let pos = self.pos();
				let u = self.[< $type $suf >]()?;
				if u != v {
					self.pos = pos;
					return Err(Error::Other { pos, source: CheckError {
						got: u,
						expected: v,
					}.into() })
				}
				Ok(())
			})*
			$(#[inline(always)] pub fn [<ptr$ptr $suf>](&mut self) -> Result<Self> {
				self.clone().at(self.[<u$ptr $suf>]()? as usize)
			})*
		}

		$(#[$trait_attrs])*
		pub trait $trait: seal::Sealed {
			$(#[doc(hidden)] fn $type(&mut self) -> Result<$type>;)*
			$(#[doc(hidden)] fn [<check_ $type>](&mut self, v: $type) -> Result<()>;)*
			$(#[doc(hidden)] fn [<ptr $ptr>](&mut self) -> Result<Self>;)*
		}

		impl<'a> $trait for Reader<'a> {
			$(#[doc(hidden)] #[inline(always)] fn $type(&mut self) -> Result<$type> {
				self.[<$type $suf>]()
			})*
			$(#[doc(hidden)] #[inline(always)] fn [<check_ $type>](&mut self, v: $type) -> Result<()> {
				self.[<check_ $type $suf>](v)
			})*
			$(#[doc(hidden)] #[inline(always)] fn [<ptr$ptr>](&mut self) -> Result<Self> {
				self.[<ptr$ptr $suf>]()
			})*
		}
	} }
}

primitives!(
	/// Allows reading little-endian primitives without `_le` suffix.
	///
	/// It is recommended to import this as `use gospel::read::Le as _;`.
	trait Le;
	_le, from_le_bytes;
	{
		u8, u16, u32, u64, u128,
		i8, i16, i32, i64, i128,
		f32, f64
	}
	{ 8, 16, 32, 64, 128 }
);
primitives!(
	/// Allows reading little-endian primitives without `_be` suffix.
	///
	/// It is recommended to import this as `use gospel::read::Be as _;`.
	trait Be;
	_be, from_be_bytes;
	{
		u8, u16, u32, u64, u128,
		i8, i16, i32, i64, i128,
		f32, f64
	}
	{ 8, 16, 32, 64, 128 }
);
