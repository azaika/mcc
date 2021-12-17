use std::fmt;
use util::Id;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Bool(bool),
    Int(i32),
    Float(f32),
    Not,
    Minus,
    Plus,
    Star,
    Slash,
    MinusDot,
    PlusDot,
    StarDot,
    SlashDot,
    Equal,
    LessGreater,
    LessEqual,
    Less,
    Greater,
    GreaterEqual,
    If,
    Then,
    Else,
    Ident(Id),
    Let,
    In,
    Rec,
    Comma,
    ArrayMake,
    Dot,
    LessMinus,
    SemiColon,
    LPar,
    RPar
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;
        match self {
            Bool(b) => write!(f, "{}", b),
            Int(i) => write!(f, "{}", i),
            Float(x) => write!(f, "{}", x),
            Not => write!(f, "not"),
            Minus => write!(f, "-"),
            Plus => write!(f, "+"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            MinusDot => write!(f, "-."),
            PlusDot => write!(f, "+."),
            StarDot => write!(f, "*."),
            SlashDot => write!(f, "/."),
            Equal => write!(f, "="),
            LessGreater => write!(f, "<>"),
            LessEqual => write!(f, "<="),
            Less => write!(f, "<"),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            If => write!(f, "if"),
            Then => write!(f, "then"),
            Else => write!(f, "else"),
            Ident(x) => write!(f, "{}", x),
            Let => write!(f, "let"),
            In => write!(f, "in"),
            Rec => write!(f, "rec"),
            Comma => write!(f, ","),
            ArrayMake => write!(f, "Array.make"),
            Dot => write!(f, "."),
            LessMinus => write!(f, "<-"),
            SemiColon => write!(f, ";"),
            LPar => write!(f, "("),
            RPar => write!(f, ")")
        }
    }
}