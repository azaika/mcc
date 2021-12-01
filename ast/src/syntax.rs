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