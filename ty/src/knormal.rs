use std::{fmt, panic};
use crate::syntax;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    Unit,
    Int,
    Float,
    Fun(Vec<Ty>, Box<Ty>),
    Tuple(Vec<Ty>),
    Array(Box<Ty>, Option<usize>)
}

impl From<syntax::Ty> for Ty {
    fn from(t: syntax::Ty) -> Self {
        use Ty::*;
        match t {
            syntax::Ty::Unit => Unit,
            syntax::Ty::Bool => Int,
            syntax::Ty::Int => Int,
            syntax::Ty::Float => Float,
            syntax::Ty::Fun(args, r) => Fun(args.into_iter().map(|x| x.into()).collect(), Box::new((*r).into())),
            syntax::Ty::Tuple(ts) => Tuple(ts.into_iter().map(|x| x.into()).collect()),
            syntax::Ty::Array(t) => Array(Box::new((*t).into()), None),
            syntax::Ty::Var(_) => panic!("Var(_) is not allowed in this context"),
        }
    }
}

impl Ty {
    fn print_block(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Ty::*;
        match self {
            Fun(_, _) | Tuple(_) => write!(f, "({})", self),
            _ => write!(f, "{}", self)
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Ty::*;
        match self {
            Unit => write!(f, "unit"),
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            Fun(args, ret) => {
                for t in args {
                    t.print_block(f)?;
                    write!(f, " -> ")?;
                }
                write!(f, "{}", *ret)
            },
            Tuple(ts) => {
                // cannot use `util::format_vec` because it does not call print_block()
                ts.first().map_or(Ok(()), |t| t.print_block(f))?;
                for t in &ts[1..ts.len()] {
                    write!(f, " * {}", t)?;
                }
                Ok(())
            },
            Array(t, s) => {
                t.print_block(f)?;
                write!(f, " array[{}]", s.map_or("?".to_string(), |x| x.to_string()))
            }
        }
    }
}

pub fn short(t: &Ty) -> &'static str {
    match t {
        Ty::Unit => "u",
        Ty::Int => "i",
        Ty::Float => "d",
        Ty::Fun(_, _) => "f",
        Ty::Tuple(_) => "t",
        Ty::Array(_, _) => "a"
    }
}

#[cfg(test)]
mod tests {
    use super::Ty::*;
    #[test]
    fn print_type() {
        let iarr = Array(Box::new(Int), Some(2));
        let iiarr = Array(Box::new(iarr.clone()), None);

        assert_eq!(iiarr.to_string(), "int array[2] array[?]");

        let fun1 = Fun(vec![Unit, iarr.clone()], Box::new(Float));
        assert_eq!(fun1.to_string(), "unit -> int array[2] -> float");

        let fun2 = Fun(vec![Float, fun1.clone()], Box::new(fun1.clone()));
        assert_eq!(fun2.to_string(), "float -> (unit -> int array[2] -> float) -> unit -> int array[2] -> float");

        let tup1 = Tuple(vec![fun1.clone(), Unit]);
        assert_eq!(tup1.to_string(), "(unit -> int array[2] -> float) * unit");

        let tup2 = Tuple(vec![iarr.clone(), Float, iiarr.clone()]);
        assert_eq!(tup2.to_string(), "int array[2] * float * int array[2] array[?]");
    }
}