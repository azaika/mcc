use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<Ty>, Box<Ty>),
    Tuple(Vec<Ty>),
    Array(Box<Ty>),
    Var(Option<Rc<RefCell<Ty>>>)
}

impl Ty {
    #[inline]
    pub fn new_var() -> Ty {
        Ty::Var(None)
    }

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
            Bool => write!(f, "bool"),
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            Fun(args, ret) => {
                for t in args {
                    t.print_block(f)?;
                    write!(f, " -> ")?;
                }
                write!(f, "{}", *ret)
            },
            Tuple(ts) => util::format_vec(f, &ts, "", " * ", ""),
            Array(t) => {
                t.print_block(f)?;
                write!(f, " array")
            },
            Var(None) => write!(f, "'?"),
            Var(Some(t)) => write!(f, "{}", t.borrow())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Ty::*;
    use std::rc::Rc;
    use std::cell::RefCell;
    #[test]
    fn print_type() {
        let iarr = Array(Box::new(Int));
        let iiarr = Array(Box::new(iarr.clone()));

        let var = Var(Some(Rc::new(RefCell::new(iarr.clone()))));
        assert_eq!(var.to_string(), "int array");

        let fun1 = Fun(vec![Unit, iarr.clone()], Box::new(Float));
        assert_eq!(fun1.to_string(), "unit -> int array -> float");

        let fun2 = Fun(vec![Float, fun1.clone()], Box::new(var));
        assert_eq!(fun2.to_string(), "float -> (unit -> int array -> float) -> int array");

        let tup1 = Tuple(vec![fun1.clone(), Unit]);
        assert_eq!(tup1.to_string(), "(unit -> int array -> float) * unit");

        let tup2 = Tuple(vec![iarr.clone(), Float, iiarr.clone()]);
        assert_eq!(tup2.to_string(), "int array * float * int array array");
    }
}