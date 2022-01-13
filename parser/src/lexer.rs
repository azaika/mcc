use crate::error::{LexError, LexErrorKind};
use crate::token::*;
use plex::lexer;
use util::{id, Spanned};

pub type Result = std::result::Result<(usize, Token, usize), LexError>;

#[derive(Debug, Clone, PartialEq)]
enum LexToken {
    Tok(Token),
    Space,
    CommentBegin,
    Error(LexErrorKind),
}

use LexToken::*;

enum CommentState {
    ReOpen,
    End,
    Continue,
}

lexer! {
    fn consume_comment(_text: 'input) -> CommentState;

    r"\(\*" => CommentState::ReOpen,
    r"\*\)" => CommentState::End,
    r"." => CommentState::Continue
}

lexer! {
    fn next_token(text: 'input) -> LexToken;

    r"[\t\n\r ]" => Space,
    r"\(\*" => CommentBegin,
    r"\(" => Tok(Token::LPar),
    r"\)" => Tok(Token::RPar),
    "true" => Tok(Token::Bool(true)),
    "false" => Tok(Token::Bool(false)),
    "not" => Tok(Token::Not),
    r"[0-9]+" => {
        if let Ok(i) = text.parse() {
            Tok(Token::Int(i))
        } else {
            Error(LexErrorKind::TooLargeInteger(text.to_owned()))
        }
    },
    r"[0-9]+(\.[0-9]*)?([eE][\+\-]?[0-9]+)?" => {
        if let Ok(f) = text.parse() {
            Tok(Token::Float(f))
        } else {
            Error(LexErrorKind::InvalidFloat(text.to_owned()))
        }
    },
    r"\-" => Tok(Token::Minus),
    r"\+" => Tok(Token::Plus),
    r"\*" => Tok(Token::Star),
    "/" => Tok(Token::Slash),
    r"\-\." => Tok(Token::MinusDot),
    r"\+\." => Tok(Token::PlusDot),
    r"\*\." => Tok(Token::StarDot),
    r"/\." => Tok(Token::SlashDot),
    "=" => Tok(Token::Equal),
    "<>" => Tok(Token::LessGreater),
    "<=" => Tok(Token::LessEqual),
    ">=" => Tok(Token::GreaterEqual),
    "<" => Tok(Token::Less),
    ">" => Tok(Token::Greater),
    "if" => Tok(Token::If),
    "then" => Tok(Token::Then),
    "else" => Tok(Token::Else),
    "let" => Tok(Token::Let),
    "in" => Tok(Token::In),
    "rec" => Tok(Token::Rec),
    "," => Tok(Token::Comma),
    "_" => Tok(Token::Ident(id::gen_tmp_var())),
    r"Array\.((create)|(make))" => Tok(Token::ArrayMake),
    r"\." => Tok(Token::Dot),
    r"<\-" => Tok(Token::LessMinus),
    r";" => Tok(Token::SemiColon),
    r"[a-z][0-9A-Za-z_]*" => Tok(Token::Ident(text.to_owned())),
    r"." => Error(LexErrorKind::UnrecognizedToken(text.to_owned()))
}

pub struct Lexer<'input> {
    original: &'input str,
    remaining: &'input str,
}

impl<'input> Lexer<'input> {
    #[inline]
    pub fn new(s: &'input str) -> Self {
        Lexer {
            original: s,
            remaining: s,
        }
    }

    fn skip_comment(&mut self) -> Option<()> {
        loop {
            use CommentState::*;
            if let Some((stat, remaining)) = consume_comment(self.remaining) {
                self.remaining = remaining;

                match stat {
                    ReOpen => {
                        self.skip_comment()?;
                        continue;
                    }
                    End => {
                        return Some(());
                    }
                    Continue => continue,
                }
            } else {
                // EOF
                return None;
            }
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((tok, remaining)) = next_token(self.remaining) {
                let lo = self.original.len() - self.remaining.len();
                let hi = self.original.len() - remaining.len();
                self.remaining = remaining;

                match tok {
                    Space => continue,
                    Tok(tok) => return Some(Ok((lo, tok, hi))),
                    CommentBegin => {
                        if let Some(_) = self.skip_comment() {
                            continue;
                        } else {
                            return Some(Err(Spanned::new(
                                LexErrorKind::UnclosedComment,
                                (lo, hi),
                            )));
                        }
                    }
                    Error(e) => return Some(Err(Spanned::new(e, (lo, hi)))),
                }
            } else {
                // EOF
                return None;
            };
        }
    }
}
