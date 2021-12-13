pub mod span;
pub mod id;

pub use span::*;
pub use id::Id;

use std::fmt;

pub fn format_vec<T: fmt::Display>(f: &mut fmt::Formatter, v: &Vec<T>, left: &str, sep: &str, right: &str) -> fmt::Result {
    write!(f, "{}", left)?;
    if !v.is_empty() {
        write!(f, "{}", v[0])?;
        for e in &v[1..v.len()] {
            write!(f, "{}{}", sep, e)?;
        }
    }
    write!(f, "{}", right)
}