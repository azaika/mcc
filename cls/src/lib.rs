#![feature(hash_drain_filter)]

pub mod alias;
mod beta;
pub mod common;
mod convert;
mod detect_doall;
mod eliminate_get;
mod fold_const;

pub use beta::beta_reduction;
pub use convert::convert;
pub use detect_doall::detect_doall;
pub use eliminate_get::eliminate_get;
pub use fold_const::fold_const;
