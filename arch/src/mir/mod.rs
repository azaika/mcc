mod convert;
pub mod mir;
mod skip_jump;
mod merge;

pub use convert::convert;
pub use skip_jump::skip_jump;
pub use merge::merge_block;