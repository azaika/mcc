use util::{Spanned, Id};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstKind {
    CUnit,
    CBool(bool),
    CInt(i32),
    CFloat(f32)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum UnOp {
    Neg,
    FNeg,
    Not
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum BinOp {
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

pub type Var = (Id, ty::Ty);

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    fvar : Var,
    args : Vec<Var>,
    body : Box<Expr>
}

#[derive(Debug, Clone, PartialEq)]
pub enum LetKind {
    Let(Var, Box<Expr>, Box<Expr>),
    LetRec(Fundef),
    LetTuple(Vec<Var>, Box<Expr>, Box<Expr>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Const(ConstKind),
    Var(Id),
    UnaryOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(LetKind),
    Array(Box<Expr>, Box<Expr>),
    Get(Box<Expr>, Box<Expr>),
    Put(Box<Expr>, Box<Expr>)
}

pub type Expr = Spanned<ExprKind>;