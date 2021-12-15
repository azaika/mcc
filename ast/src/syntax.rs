use std::fmt;

use util::{Spanned, Id};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstKind {
    CUnit,
    CBool(bool),
    CInt(i32),
    CFloat(f32)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum UnOpKind {
    Neg,
    FNeg,
    Not
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
    FDiv,
    Eq,
    LE
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
    LetTuple(Vec<Decl>, Box<Expr>, Box<Expr>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Const(ConstKind),
    Var(Id),
    UnOp(UnOpKind, Box<Expr>),
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(LetKind),
    Tuple(Vec<Expr>),
    App(Box<Expr>, Vec<Expr>),
    Array(Box<Expr>, Box<Expr>),
    Get(Box<Expr>, Box<Expr>),
    Put(Box<Expr>, Box<Expr>, Box<Expr>)
}

pub type Expr = Spanned<ExprKind>;

pub fn concat(e1: Expr, e2: Expr) -> Box<Expr> {
    use ExprKind::*;
    match e1.item {
        Let(l) => {
            let l = match l {
                LetKind::Let(d, e1, e2_) => LetKind::Let(d, e1, concat(*e2_, e2)),
                LetKind::LetRec(fundef, e) => LetKind::LetRec(fundef, concat(*e, e2)),
                LetKind::LetTuple(ds, e1, e2_) => LetKind::LetTuple(ds, e1, concat(*e2_, e2)),
            };
            Box::new(util::Spanned::new(Let(l), e1.loc))
        },
        _ => Box::new(e2)
    }
}

impl ExprKind {
    fn format_indented(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        // print indentation
        let indent = |level: usize| "    ".repeat(level);
        write!(f, "{}", indent(level))?;

        use ExprKind::*;
        match self {
            Const(c) => write!(f, "{:?}\n", c),
            Var(v) => write!(f, "Var({})\n", v),
            UnOp(op, e) => {
                write!(f, "{:?}\n", op)?;
                e.item.format_indented(f, level + 1)
            },
            BinOp(op, e1, e2) => {
                write!(f, "{:?}\n", op)?;
                e1.item.format_indented(f, level + 1)?;
                e2.item.format_indented(f, level + 1)
            },
            If(cond, e1, e2) => {
                write!(f, "If:\n")?;
                cond.item.format_indented(f, level + 1)?;
                write!(f, "{}Then:\n", indent(level))?;
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
                    LetTuple(decls, e1, e2) => {
                        write!(f, "LetTuple: ")?;
                        util::format_vec(f, &decls, "[", ", ", "]")?;
                        write!(f, "\n")?;
                        e1.item.format_indented(f, level + 1)?;
                        e2.item.format_indented(f, level)
                    }
                } 
            },
            Tuple(es) => {
                write!(f, "Tuple:\n")?;
                for (i, e) in es.iter().enumerate() {
                    write!(f, "{}[{}]:\n", indent(level), i)?;
                    e.item.format_indented(f, level + 1)?;
                }
                Ok(())
            },
            App(func, args) => {
                write!(f, "App:\n{}func =\n", indent(level + 1))?;
                func.item.format_indented(f, level + 2)?;
                write!(f, "{}args =\n", indent(level + 1))?;
                for (i, e) in args.iter().enumerate() {
                    write!(f, "{}[{}]:\n", indent(level + 1), i)?;
                    e.item.format_indented(f, level + 2)?;
                }
                Ok(())
            },
            Array(num, init) => {
                write!(f, "Array:\n{}func =\n", indent(level + 1))?;
                num.item.format_indented(f, level + 2)?;
                write!(f, "{}init =\n", indent(level + 1))?;
                init.item.format_indented(f, level + 2)
            },
            Get(arr, idx) => {
                write!(f, "Get:\n{}dest =\n", indent(level + 1))?;
                arr.item.format_indented(f, level + 2)?;
                write!(f, "{}idx =\n", indent(level + 1))?;
                idx.item.format_indented(f, level + 2)
            },
            Put(arr, idx, e) => {
                write!(f, "Put:\n{}dest =\n", indent(level + 1))?;
                arr.item.format_indented(f, level + 2)?;
                write!(f, "{}idx =\n", indent(level + 1))?;
                idx.item.format_indented(f, level + 2)?;
                write!(f, "{}content =\n", indent(level + 1))?;
                e.item.format_indented(f, level + 2)
            }
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format_indented(f, 0)
    }
}