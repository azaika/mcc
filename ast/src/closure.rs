use std::fmt;

use util::{Id, Spanned};

use crate::knormal;

pub type ConstKind = knormal::ConstKind;
pub type UnOpKind = knormal::UnOpKind;
pub type BinOpKind = knormal::BinOpKind;
pub type Decl = knormal::Decl;
pub type IfKind = knormal::IfKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Label(pub Id);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ".L({})", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    pub fvar: Decl,
    pub args: Vec<Decl>,
    pub formal_fv: Vec<Decl>,
    pub body: Box<Expr>,
}

impl Fundef {
    fn format_indented(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        // print indentation
        let indent = |level: usize| "    ".repeat(level);
        write!(f, "{}Fundef {}", indent(level), self.fvar)?;
        write!(f, "\n{}args: ", indent(level + 1))?;
        util::format_vec(f, &self.args, "[", ", ", "]")?;
        write!(f, "\n{}formal_fv: ", indent(level + 1))?;
        util::format_vec(f, &self.formal_fv, "[", ", ", "]")?;
        write!(f, "\n{}body:\n", indent(level + 1))?;
        self.body.item.format_indented(f, level + 2)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Const(ConstKind),
    Var(Id),
    UnOp(UnOpKind, Id),
    BinOp(BinOpKind, Id, Id),
    If(IfKind, Id, Id, Box<Expr>, Box<Expr>),
    Let(Decl, Box<Expr>, Box<Expr>),
    Tuple(Vec<Id>),
    CallDir(Label, Vec<Id>),
    CallCls(Id, Vec<Id>),
    CreateArray(Id, Id),
    ExtArray(Label),
    ArrayGet(Id, Id),
    ArrayPut(Id, Id, Id),
    TupleGet(Id, usize),
    Loop {
        vars: Vec<Decl>,
        init: Vec<Id>,
        body: Box<Expr>,
    },
    Continue(Vec<(Id, Id)>), // only in Loop.body
    MakeCls(Label, Vec<Id>), // (label, actual_fv)
    Assign(Label, Id),
    Load(Label),
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
            Var(v) => write!(f, "Var {v}\n"),
            ExtArray(x) => write!(f, "ExtArray {x}\n"),
            UnOp(op, x) => write!(f, "{:?} {x}\n", op),
            BinOp(op, x, y) => write!(f, "{:?} {x}, {y}\n", op),
            If(kind, x, y, e1, e2) => {
                write!(f, "{:?} {x}, {y}:\n", kind)?;
                e1.item.format_indented(f, level + 1)?;
                write!(f, "{}Else:\n", indent(level))?;
                e2.item.format_indented(f, level + 1)
            }
            Let(d, e1, e2) => {
                write!(f, "Let: {d}\n")?;
                e1.item.format_indented(f, level + 1)?;
                e2.item.format_indented(f, level)
            }
            Tuple(xs) => {
                write!(f, "Tuple ")?;
                util::format_vec(f, xs, "(", ", ", ")")?;
                write!(f, "\n")
            }
            CallDir(func, args) => {
                write!(f, "CallDir {func}")?;
                util::format_vec(f, args, "(", ", ", ")")?;
                write!(f, "\n")
            }
            CallCls(func, args) => {
                write!(f, "CallCls {func}")?;
                util::format_vec(f, args, "(", ", ", ")")?;
                write!(f, "\n")
            }
            CreateArray(num, init) => write!(f, "CreateArray {num}, {init}\n"),
            ArrayGet(arr, idx) => write!(f, "ArrayGet {arr}, {idx}\n"),
            ArrayPut(arr, idx, x) => write!(f, "ArrayPut {arr}, {idx}, {x}\n"),
            TupleGet(arr, idx) => write!(f, "TupleGet {arr}.{idx}\n"),
            Loop { vars, init, body } => {
                write!(f, "Loop:\n{}vars = ", indent(level + 1))?;
                util::format_vec(f, vars, "[", ", ", "]")?;
                write!(f, "\n{}init = ", indent(level + 1))?;
                util::format_vec(f, init, "[", ", ", "]")?;
                write!(f, "\n{}body =\n", indent(level + 1))?;
                body.item.format_indented(f, level + 2)
            }
            Continue(xs) => {
                write!(f, "Continue ")?;
                util::format_vec(f, &xs.iter().map(|(_, x)| x).collect(), "[", ", ", "]")?;
                write!(f, "\n")
            }
            MakeCls(l, actual_fv) => {
                write!(f, "MakeCls {} @ ", l)?;
                util::format_vec(f, actual_fv, "[", ", ", "]")?;
                write!(f, "\n")
            }
            Assign(x, y) => write!(f, "Assign {x}, {y}"),
            Load(x) => write!(f, "Load {x}"),
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format_indented(f, 0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
    pub name: Label,
    pub t: ty::knormal::Ty,
    pub init: Box<Expr>,
}

impl Global {
    fn format_indented(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        let indent = |level: usize| "    ".repeat(level);

        write!(f, "{}Global: ({}, {})\n", indent(level), self.name, self.t)?;
        self.init.item.format_indented(f, level + 1)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub globals: Vec<Global>,
    pub fundefs: Vec<Fundef>,
    pub main: Box<Expr>,
}

impl Program {
    pub fn new() -> Self {
        let dummy = Spanned::new(ExprKind::Var("!!dummy!!".to_string()), (0, 0));
        Self {
            globals: vec![],
            fundefs: vec![],
            main: Box::new(dummy),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[Globals]\n")?;
        for g in &self.globals {
            g.format_indented(f, 1)?;
        }
        write!(f, "\n[Fundefs]\n")?;
        for fundef in &self.fundefs {
            fundef.format_indented(f, 1)?;
            write!(f, "\n")?;
        }
        write!(f, "\n[main]\n")?;
        self.main.item.format_indented(f, 1)
    }
}
