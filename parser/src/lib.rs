pub mod error;
mod lexer;
mod token;

pub use error::ParseError as Error;

use ast::syntax::Expr;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub grammer);

#[inline]
pub fn parse<'input>(src: &'input str) -> Result<Expr, Error> {
    let parser = grammer::ExprParser::new();
    let lex = lexer::Lexer::new(src);

    parser.parse(lex).map(|x| *x).map_err(error::from_lalrpop)
}
