mod convert;
mod alpha;
mod beta;
mod flatten;
mod fold_const;
mod inlining;
mod eliminate;
mod cse;

pub use convert::convert;
pub use alpha::to_alpha_form;
pub use alpha::TyMap;
pub use beta::beta_reduction;
pub use flatten::flatten_let;
pub use fold_const::fold_const;
pub use inlining::inlining;
pub use eliminate::eliminate;
pub use cse::cse;