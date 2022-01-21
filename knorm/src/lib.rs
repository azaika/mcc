mod alpha;
mod beta;
mod compress_onehot_if;
mod convert;
mod cse;
mod detect_loop;
mod eliminate;
mod flatten;
mod fold_const;
mod inlining;
mod simplify_loop;

pub use alpha::to_alpha_form;
pub use alpha::TyMap;
pub use beta::beta_reduction;
pub use compress_onehot_if::compress_onehot_if;
pub use convert::convert;
pub use cse::cse;
pub use detect_loop::detect_loop;
pub use eliminate::eliminate;
pub use flatten::flatten_let;
pub use fold_const::fold_const;
pub use inlining::inlining;
pub use simplify_loop::simplify_loop;
