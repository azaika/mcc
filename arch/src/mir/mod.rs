mod convert;
mod eliminate;
mod fold_const;
mod merge;
pub mod mir;
mod peephole;
mod skip_jump;

pub use convert::convert;
pub use eliminate::eliminate;
pub use fold_const::fold_const;
pub use merge::merge_block;
pub use peephole::faddmul;
pub use skip_jump::skip_jump;
