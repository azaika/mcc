use thiserror::Error;
use lalrpop_util;

use util::Spanned;
use crate::token;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum LexErrorKind {
    #[error("unclosed comment")]
    UnclosedComment,
    #[error("unrecognized token `{0}`")]
    UnrecognizedToken(String),
    #[error("integer constant `{0}` is too large")]
    TooLargeInteger(String),
    #[error("floating constant `{0}` exceeds range of IEEE754 single precision")]
    InvalidFloat(String)
}

pub type LexError = Spanned<LexErrorKind>;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParseErrorKind {
    #[error("parse error: unexpected end of file")]
    Eof,
    #[error("lexer error: {0}")]
    Lexical(LexErrorKind),
    #[error("parse error: found extra token `{0}`")]
    ExtraToken(token::Token),
    #[error("parse error: invalid token `{0}`")]
    InvalidToken(token::Token),
    #[error("parse error: unrecognized token `{0}`, expected `{1}`")]
    UnrecognizedToken(token::Token, String)
}

pub type ParseError = Spanned<ParseErrorKind>;

type LalrpopError = lalrpop_util::ParseError<usize, token::Token, LexError>;

pub fn from_lalrpop(err: LalrpopError) -> ParseError {
    match err {
        // TODO: Are there cases where this isn't an EOF?
        LalrpopError::InvalidToken { location } => Spanned::new(ParseErrorKind::Eof, (location, location)),
        LalrpopError::ExtraToken { token: (lo, tok, hi) } => Spanned::new(ParseErrorKind::ExtraToken(tok), (lo, hi)),
        LalrpopError::User { error } => {
            error.map(ParseErrorKind::Lexical)
        },
        LalrpopError::UnrecognizedToken {
            token: (lo, tok, hi),
            expected
        } => {
            // take only first expected candidate
            assert!(!expected.is_empty());
            let expected = expected[0].clone();

            Spanned::new(ParseErrorKind::UnrecognizedToken(tok, expected), (lo, hi))
        }
        LalrpopError::UnrecognizedEOF { location, .. } => Spanned::new(ParseErrorKind::Eof, (location, location)),
    }
}
