/*!
A crate for incremental reading and writing of binary files.

In addition to convenient zero-copy reading, the [`gospel::read`](crate::read) module supports
seeking, forking, and testing for testing for magic numbers. It also comes with
the separate `gospel-dump` crate for writing hex dumps, which is useful when
analyzing an unknown format.

The [`gospel::write`](crate::write) module supports the dual of seeking, namely delayed writes
with labels.
*/

pub mod read;
pub mod write;

pub mod dump;
