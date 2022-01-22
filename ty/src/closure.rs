use crate::knormal;
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ty {
    Unit,
    Int,
    Float,
    Fun(Vec<Ty>, Box<Ty>),
    TuplePtr(Vec<Ty>),
    Tuple(Vec<Ty>),
    Array(Box<Ty>, usize),
    ArrayPtr(Box<Ty>),
}

impl From<knormal::Ty> for Ty {
    fn from(t: knormal::Ty) -> Self {
        use Ty::*;
        match t {
            knormal::Ty::Unit => Unit,
            knormal::Ty::Int => Int,
            knormal::Ty::Float => Float,
            knormal::Ty::Fun(args, r) => Fun(
                args.into_iter().map(|x| x.into()).collect(),
                Box::new((*r).into()),
            ),
            knormal::Ty::Tuple(ts) => TuplePtr(ts.into_iter().map(|x| x.into()).collect()),
            knormal::Ty::Array(t) => ArrayPtr(Box::new((*t).into())),
        }
    }
}

impl Ty {
    fn print_block(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Ty::*;
        match self {
            Fun(_, _) | TuplePtr(_) | Tuple(_) => write!(f, "({})", self),
            _ => write!(f, "{}", self),
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Self::Array(..) | Self::ArrayPtr(..) => true,
            _ => false,
        }
    }

    pub fn elem_t(&self) -> &Self {
        match self {
            Self::Array(t, _) | Self::ArrayPtr(t) => t,
            _ => panic!(),
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
            }
            Tuple(ts) => {
                // cannot use `util::format_vec` because it does not call print_block()
                ts.first().map_or(Ok(()), |t| t.print_block(f))?;
                for t in &ts[1..ts.len()] {
                    write!(f, " * {}", t)?;
                }
                Ok(())
            }
            TuplePtr(ts) => {
                write!(f, "(")?;
                ts.first().map_or(Ok(()), |t| t.print_block(f))?;
                for t in &ts[1..ts.len()] {
                    write!(f, " * {}", t)?;
                }
                write!(f, ") ptr")
            }
            Array(t, s) => {
                t.print_block(f)?;
                write!(f, " array[{s}]")
            }
            ArrayPtr(t) => {
                t.print_block(f)?;
                write!(f, " array ptr")
            }
        }
    }
}

impl Ty {
    pub fn short(&self) -> &'static str {
        match self {
            Ty::Unit => "u",
            Ty::Int => "i",
            Ty::Float => "d",
            Ty::Fun(_, _) => "f",
            Ty::TuplePtr(_) | Ty::Tuple(_) => "t",
            Ty::Array(..) | Ty::ArrayPtr(..) => "a",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Ty::*;
    #[test]
    fn print_type() {
        let iarr = Array(Box::new(Int), 2usize);
        let iiarr = ArrayPtr(Box::new(iarr.clone()));

        assert_eq!(iiarr.to_string(), "int array[2] array ptr");

        let fun1 = Fun(vec![Unit, iarr.clone()], Box::new(Float));
        assert_eq!(fun1.to_string(), "unit -> int array[2] -> float");

        let fun2 = Fun(vec![Float, fun1.clone()], Box::new(fun1.clone()));
        assert_eq!(
            fun2.to_string(),
            "float -> (unit -> int array[2] -> float) -> unit -> int array[2] -> float"
        );

        let tup1 = Tuple(vec![fun1.clone(), Unit]);
        assert_eq!(tup1.to_string(), "(unit -> int array[2] -> float) * unit");

        let tup2 = TuplePtr(vec![iarr.clone(), Float, iiarr.clone()]);
        assert_eq!(
            tup2.to_string(),
            "(int array[2] * float * int array[2] array ptr) ptr"
        );
    }
}
