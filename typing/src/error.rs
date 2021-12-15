use thiserror::Error;
use util::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct UnifyError(pub ty::Ty, pub ty::Ty);

#[derive(Debug, Clone, PartialEq, Error)]
#[error("type error: expected `{expected}`, found `{found}`")]
pub struct TypeError {
    pub expected: ty::Ty,
    pub found: ty::Ty,
    pub span: Span
}
