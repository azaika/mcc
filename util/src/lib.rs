pub mod id;
pub mod span;

pub use id::Id;
pub use span::*;

use std::fmt;

pub fn format_vec<T: fmt::Display>(
    f: &mut fmt::Formatter,
    v: &Vec<T>,
    left: &str,
    sep: &str,
    right: &str,
) -> fmt::Result {
    write!(f, "{}", left)?;
    if !v.is_empty() {
        write!(f, "{}", v[0])?;
        for e in &v[1..v.len()] {
            write!(f, "{}{}", sep, e)?;
        }
    }
    write!(f, "{}", right)
}

pub use fnv::FnvHashMap as Map;
pub use fnv::FnvHashSet as Set;

pub use fnv::FnvBuildHasher as Hasher;

pub fn restore<V>(m: &mut Map<Id, V>, key: &str, t: Option<V>) {
    match t {
        Some(t) => m.insert(key.to_string(), t),
        None => m.remove(key),
    };
}
