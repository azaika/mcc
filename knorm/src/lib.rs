mod convert;
mod alpha;
mod beta;
mod flatten;
pub mod inlining;

pub use convert::convert;
pub use alpha::to_alpha_form;
pub use alpha::TyMap;
pub use beta::beta_reduction;
pub use flatten::flatten_let;