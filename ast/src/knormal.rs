use std::fmt;

use util::{Spanned, Id};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstKind {
    CUnit,
    CInt(i32),
    CFloat(f32)
}

impl From<i32> for ConstKind {
    fn from(i: i32) -> Self {
        Self::CInt(i)
    }
}
impl From<f32> for ConstKind {
    fn from(f: f32) -> Self {
        Self::CFloat(f)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum UnOpKind {
    Neg,
    FNeg
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    FAdd,
    FSub,
    Mul,
    Div,
    FMul,
    FDiv
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub name : Id,
    pub t : ty::Ty
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.name, self.t)
    }
}

impl Decl {
    pub fn new(name : Id, t : ty::Ty) -> Self {
        Self {
            name,
            t
        }
    }

    pub fn gen_uniq(t: ty::Ty) -> Self {
        Self {
            name: util::id::gen_uniq_with(ty::short(&t)),
            t
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    pub fvar : Decl,
    pub args : Vec<Decl>,
    pub body : Box<Expr>
}

#[derive(Debug, Clone, PartialEq)]
pub enum LetKind {
    Let(Decl, Box<Expr>, Box<Expr>),
    LetRec(Fundef, Box<Expr>),
    LetTuple(Vec<Decl>, Id, Box<Expr>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfKind {
    IfEq,
    IfLE
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Const(ConstKind),
    Var(Id),
    UnOp(UnOpKind, Id),
    BinOp(BinOpKind, Id, Id),
    If(IfKind, Id, Id, Box<Expr>, Box<Expr>),
    Let(LetKind),
    Tuple(Vec<Id>),
    App(Id, Vec<Id>),
    ExtApp(Id, Vec<Id>),
    CreateArray(Id, Id),
    ExtArray(Id),
    Get(Id, Id),
    Put(Id, Id, Id)
}

pub type Expr = Spanned<ExprKind>;

impl ExprKind {
    fn format_indented(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        // print indentation
        let indent = |level: usize| "    ".repeat(level);
        write!(f, "{}", indent(level))?;

        use ExprKind::*;
        match self {
            Const(c) => write!(f, "{:?}\n", c),
            Var(v) => write!(f, "Var {}\n", v),
            ExtArray(x) => write!(f, "ExtArray {}\n", x),
            UnOp(op, x) => write!(f, "{:?} {}\n", op, x),
            BinOp(op, x, y) => write!(f, "{:?} {}, {}\n", op, x, y),
            If(kind, x, y, e1, e2) => {
                write!(f, "{:?} {}, {}:\n", kind, x, y)?;
                e1.item.format_indented(f, level + 1)?;
                write!(f, "{}Else:\n", indent(level))?;
                e2.item.format_indented(f, level + 1)
            },
            Let(l) => {
                use LetKind::*;
                match l {
                    Let(d, e1, e2) => {
                        write!(f, "Let: {}\n", d)?;
                        e1.item.format_indented(f, level + 1)?;
                        e2.item.format_indented(f, level)
                    }
                    LetRec(fundef, e) => {
                        write!(f, "LetRec: {}\n{}args = ", fundef.fvar, indent(level + 1))?;
                        util::format_vec(f, &fundef.args, "[", ", ", "]")?;
                        write!(f, "\n{}body =\n", indent(level + 1))?;
                        fundef.body.item.format_indented(f, level + 2)?;
                        e.item.format_indented(f, level)
                    },
                    LetTuple(decls, x, e) => {
                        write!(f, "LetTuple ")?;
                        util::format_vec(f, &decls, "(", ", ", ")")?;
                        write!(f, ":\n{}{}\n", indent(level + 1), x)?;
                        e.item.format_indented(f, level)
                    }
                } 
            },
            Tuple(xs) => {
                write!(f, "Tuple ")?;
                util::format_vec(f, xs, "(", ", ", ")")?;
                write!(f, "\n")
            },
            App(func, args) => {
                write!(f, "App {}", func)?;
                util::format_vec(f, args, "(", ", ", ")")?;
                write!(f, "\n")
            },
            ExtApp(func, args) => {
                write!(f, "ExtApp {}", func)?;
                util::format_vec(f, args, "(", ", ", ")")?;
                write!(f, "\n")
            },
            CreateArray(num, init) => write!(f, "CreateArray {}, {}\n", num, init),
            Get(arr, idx) => write!(f, "Get {}, {}\n", arr, idx),
            Put(arr, idx, e) => write!(f, "Put {}, {}, {}\n", arr, idx, e)
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format_indented(f, 0)
    }
}