use thiserror::Error;
use util::Span;

use ty::syntax::Ty;

#[derive(Debug, Clone, PartialEq)]
pub struct UnifyError(pub Ty, pub Ty);

#[derive(Debug, Clone, PartialEq, Error)]
#[error("type error: expected `{expected}`, found `{found}`")]
pub struct TypeError {
    pub expected: Ty,
    pub found: Ty,
    pub span: Span,
}
