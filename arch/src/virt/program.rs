use std::fmt;
use util::{Id, Spanned};

use ast::closure;

pub type Ty = closure::Ty;

pub type Label = closure::Label;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Var(Id),
    Imm(i16),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Var(v) => write!(f, "V({v})"),
            Value::Imm(i) => write!(f, "I({i})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOpKind {
    Neg,
    FNeg,
    FAbs,
    FSin,
    FCos,
    FAtan,
    FSqrt,
    Ftoi,
    Itof,
    FFloor,
    FHalf,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntOpKind {
    Add,
    Sub,
    Mul16,
    Shl,
    Shr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FloatOpKind {
    FAdd,
    FSub,
    FMul,
    FDiv,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfKind {
    IfEq,
    IfLE,
    IfGE,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    pub name: Id,
    pub args: Vec<Id>,
    pub formal_fv: Vec<Id>,
    pub body: Box<Expr>,
}

impl Fundef {
    fn format_indented(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        // print indentation
        let indent = |level: usize| "    ".repeat(level);
        write!(f, "{}Fundef {}", indent(level), self.name)?;
        write!(f, "\n{}args: ", indent(level + 1))?;
        util::format_vec(f, &self.args, "[", ", ", "]")?;
        write!(f, "\n{}formal_fv: ", indent(level + 1))?;
        util::format_vec(f, &self.formal_fv, "[", ", ", "]")?;
        write!(f, "\n{}body:\n", indent(level + 1))?;
        self.body.item.format_indented(f, level + 2, false)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Nop,
    Var(Id),
    Li(i32),
    FLi(f32),
    GetLabel(Label),
    LoadLabel(Label),
    UnOp(UnOpKind, Id),
    IntOp(IntOpKind, Id, Value),
    FloatOp(FloatOpKind, Id, Id),
    If(IfKind, Id, Value, Box<Expr>, Box<Expr>),
    IfF(IfKind, Id, Id, Box<Expr>, Box<Expr>),
    Let(Option<Id>, Box<Expr>, Box<Expr>),
    CallDir(Label, Vec<Value>),
    CallCls(Id, Vec<Value>),
    AllocHeap(Value),
    Lw(Id, Value),
    Sw(Id, Value, Id), // (dest, offset, v)
    Loop {
        vars: Vec<Id>,
        init: Vec<Value>,
        body: Box<Expr>,
    },
    Continue(Vec<(Id, Id)>), // only in Loop.body
    In,
    Out(Id),
    Save(Id, Id), // (tag, data)
    Restore(Id)
}

pub type Expr = Spanned<ExprKind>;

pub trait ExprMap {
    type Lifted;
    fn map<F: FnMut(Box<Self::Lifted>) -> Box<Self::Lifted>>(self, f: F) -> Self
    where
        Self: Sized;
    fn map_ref<F: FnMut(&Self::Lifted) -> ()>(&self, f: F) -> ();
}

impl ExprMap for ExprKind {
    type Lifted = Expr;

    fn map<F: FnMut(Box<Self::Lifted>) -> Box<Self::Lifted>>(self, mut f: F) -> Self
    where
        Self: Sized,
    {
        use ExprKind::*;
        match self {
            If(kind, x, y, e1, e2) => If(kind, x, y, f(e1), f(e2)),
            Let(v, e1, e2) => Let(v, f(e1), f(e2)),
            Loop { vars, init, body } => Loop {
                vars,
                init,
                body: f(body),
            },
            e => e,
        }
    }

    fn map_ref<F: FnMut(&Self::Lifted) -> ()>(&self, mut f: F) -> () {
        use ExprKind::*;
        match self {
            If(_, _, _, e1, e2) | Let(_, e1, e2) => {
                f(e1);
                f(e2);
            }
            Loop { body, .. } => f(body),
            _ => (),
        }
    }
}

impl ExprKind {
    fn format_indented(&self, f: &mut fmt::Formatter, level: usize, noindent: bool) -> fmt::Result {
        // print indentation
        let indent = |level: usize| "    ".repeat(level);
        if !noindent {
            write!(f, "{}", indent(level))?;
        }

        use ExprKind::*;
        match self {
            Nop => write!(f, "Nop\n"),
            Var(v) => write!(f, "Var {v}\n"),
            Li(x) => write!(f, "Li {x}\n"),
            FLi(x) => write!(f, "FLi {x}\n"),
            GetLabel(label) => write!(f, "GetLabel {label}\n"),
            LoadLabel(label) => write!(f, "LoadLabel {label}\n"),
            UnOp(op, x) => write!(f, "{:?} {x}\n", op),
            IntOp(op, x, y) => write!(f, "{:?} {x}, {y}\n", op),
            FloatOp(op, x, y) => write!(f, "{:?} {x}, {y}\n", op),
            If(kind, x, y, e1, e2) => {
                write!(f, "If {:?} {x}, {y}:\n", kind)?;
                e1.item.format_indented(f, level + 1, false)?;
                write!(f, "{}Else:\n", indent(level))?;
                e2.item.format_indented(f, level + 1, false)
            }
            IfF(kind, x, y, e1, e2) => {
                write!(f, "IfF {:?} {x}, {y}:\n", kind)?;
                e1.item.format_indented(f, level + 1, false)?;
                write!(f, "{}Else:\n", indent(level))?;
                e2.item.format_indented(f, level + 1, false)
            }
            Let(d, e1, e2) => {
                if let Some(d) = d {
                    write!(f, "Let: {d}\n")?;
                    e1.item.format_indented(f, level + 1, false)?;
                } else {
                    e1.item.format_indented(f, level, true)?;
                }
                e2.item.format_indented(f, level, false)
            }
            AllocHeap(x) => write!(f, "AllocHeap {x}\n"),
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
            Loop { vars, init, body } => {
                write!(f, "Loop:\n{}vars = ", indent(level))?;
                util::format_vec(f, vars, "[", ", ", "]")?;
                write!(f, "\n{}init = ", indent(level))?;
                util::format_vec(f, init, "[", ", ", "]")?;
                write!(f, "\n{}body =\n", indent(level))?;
                body.item.format_indented(f, level + 1, false)
            }
            Continue(xs) => {
                write!(f, "Continue ")?;
                util::format_vec(f, &xs.iter().map(|(_, x)| x).collect(), "[", ", ", "]")?;
                write!(f, "\n")
            }
            Lw(x, y) => write!(f, "Lw {x}, {y}\n"),
            Sw(x, y, z) => write!(f, "Sw {x}, {y}, {z}\n"),
            In => write!(f, "In\n"),
            Out(x) => write!(f, "Out {x}\n"),
            Save(tag, x) => write!(f, "Save ${tag} <- `{x}`\n"),
            Restore(tag) => write!(f, "Restore ${tag}\n"),
        }
    }

    pub fn dummy() -> Expr {
        Spanned::new(ExprKind::Var("!!dummy!!".to_string()), (0, 0))
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format_indented(f, 0, false)
    }
}

pub type TyMap = util::Map<Id, Ty>;

#[derive(Debug, Clone, PartialEq)]
pub enum GlobalData {
    GInt16(i16),
    GInt32(i32),
    GFloat(f32),
    GSpace(usize),
}

impl GlobalData {
    pub fn is_space(&self) -> bool {
        match self {
            Self::GSpace(_) => true,
            _ => false,
        }
    }
    pub fn get_space(&self) -> Option<usize> {
        match self {
            Self::GSpace(s) => Some(*s),
            _ => None,
        }
    }
    pub fn is_int(&self) -> bool {
        match self {
            Self::GInt16(_) | Self::GInt32(_) => true,
            _ => false,
        }
    }
    pub fn get_int(&self) -> Option<i32> {
        match self {
            Self::GInt16(i) => Some(*i as i32),
            Self::GInt32(i) => Some(*i),
            _ => None,
        }
    }
}

impl fmt::Display for GlobalData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::GInt16(i) => write!(f, "I16({i})"),
            Self::GInt32(i) => write!(f, "Int({i})"),
            Self::GFloat(x) => write!(f, "Float({x})"),
            Self::GSpace(s) => write!(f, "Space({s})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub tyenv: TyMap,
    pub globals: Vec<(Id, GlobalData)>,
    pub fundefs: Vec<Fundef>,
    pub main: Box<Expr>,
}

impl Program {
    pub fn new() -> Self {
        let dummy = Spanned::new(ExprKind::Var("!!dummy!!".to_string()), (0, 0));
        Self {
            tyenv: TyMap::default(),
            globals: vec![],
            fundefs: vec![],
            main: Box::new(dummy),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[TyMaps]\n")?;
        for (x, t) in &self.tyenv {
            writeln!(f, "    {x}: {t}")?;
        }
        write!(f, "\n[Globals]\n")?;
        for (g, s) in &self.globals {
            writeln!(f, "    ({g}: {}, {s})", self.tyenv.get(g).unwrap())?;
        }
        write!(f, "\n[Fundefs]\n")?;
        for fundef in &self.fundefs {
            fundef.format_indented(f, 1)?;
            write!(f, "\n")?;
        }
        write!(f, "\n[main]\n")?;
        self.main.item.format_indented(f, 1, false)
    }
}
