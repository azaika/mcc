mod beta;
mod convert;
mod detect_doall;
mod fold_const;

pub use beta::beta_reduction;
pub use convert::convert;
pub use detect_doall::detect_doall;
pub use fold_const::fold_const;
