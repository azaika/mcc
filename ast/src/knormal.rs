use std::{fmt, hash::Hash};

use ty::knormal as ty;
use util::{Id, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum ConstKind {
    CUnit,
    CInt(i32),
    CFloat(f32),
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

impl Hash for ConstKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            ConstKind::CInt(i) => i.hash(state),
            ConstKind::CFloat(d) => d.to_bits().hash(state),
            _ => (),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum UnOpKind {
    Neg,
    FNeg,
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
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Decl {
    pub name: Id,
    pub t: ty::Ty,
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.name, self.t)
    }
}

impl Decl {
    pub fn new(name: Id, t: ty::Ty) -> Self {
        Self { name, t }
    }

    pub fn gen_uniq(t: ty::Ty) -> Self {
        Self {
            name: util::id::gen_tmp_var_with(t.short()),
            t,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    pub fvar: Decl,
    pub args: Vec<Decl>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IfKind {
    IfEq,
    IfLE,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Const(ConstKind),
    Var(Id),
    UnOp(UnOpKind, Id),
    BinOp(BinOpKind, Id, Id),
    If(IfKind, Id, Id, Box<Expr>, Box<Expr>),
    Let(Decl, Box<Expr>, Box<Expr>),
    LetRec(Fundef, Box<Expr>),
    Tuple(Vec<Id>),
    App(Id, Vec<Id>),
    ExtApp(Id, Vec<Id>),
    CreateArray(Id, Id),
    ExtArray(Id),
    ArrayGet(Id, Id),
    ArrayPut(Id, Id, Id),
    TupleGet(Id, usize),
    Loop {
        vars: Vec<Decl>,
        init: Vec<Id>,
        body: Box<Expr>,
    },
    Continue(Vec<(Id, Id)>), // only in Loop.body
    Asm(String, Vec<Id>),
    AsmE(String, Vec<Id>),
}

pub type Expr = Spanned<ExprKind>;

type Map = util::Map<Id, Id>;

// `e` must be alpha formed
pub fn rename(mut e: Box<Expr>, env: &Map) -> Box<Expr> {
    macro_rules! map {
        ($name: expr) => {
            if let Some(_x) = env.get(&$name) {
                _x.clone()
            } else {
                $name.clone()
            }
        };
    }

    use ExprKind::*;
    e.item = match e.item {
        Var(x) => Var(map!(x)),
        UnOp(op, x) => UnOp(op, map!(x)),
        BinOp(op, x, y) => BinOp(op, map!(x), map!(y)),
        If(kind, x, y, e1, e2) => {
            let e1 = rename(e1, env);
            let e2 = rename(e2, env);
            If(kind, map!(x), map!(y), e1, e2)
        }
        Let(decl, e1, e2) => {
            let e1 = rename(e1, env);
            let e2 = rename(e2, env);
            Let(Decl::new(map!(decl.name), decl.t), e1, e2)
        }
        LetRec(fundef, e2) => {
            let decl = fundef.fvar;
            let body = rename(fundef.body, env);
            let args = fundef
                .args
                .into_iter()
                .map(|d| Decl::new(map!(d.name), d.t))
                .collect();
            let e2 = rename(e2, env);

            let fundef = Fundef {
                fvar: Decl::new(map!(decl.name), decl.t),
                args,
                body,
            };
            LetRec(fundef, e2)
        }
        Tuple(xs) => Tuple(xs.into_iter().map(|x| map!(x)).collect()),
        App(f, args) => App(map!(f), args.into_iter().map(|x| map!(x)).collect()),
        ExtApp(f, args) => ExtApp(f, args.into_iter().map(|x| map!(x)).collect()),
        CreateArray(num, init) => CreateArray(map!(num), map!(init)),
        ArrayGet(x, y) => ArrayGet(map!(x), map!(y)),
        ArrayPut(x, y, z) => ArrayPut(map!(x), map!(y), map!(z)),
        TupleGet(x, idx) => TupleGet(map!(x), idx),
        Loop { vars, init, body } => {
            let vars = vars
                .into_iter()
                .map(|d| Decl::new(map!(d.name), d.t))
                .collect();
            let init = init.into_iter().map(|x| map!(x)).collect();
            let body = rename(body, env);

            Loop { vars, init, body }
        }
        Continue(xs) => Continue(xs.into_iter().map(|x| (map!(x.0), map!(x.1))).collect()),
        Asm(inst, args) => Asm(inst, args.into_iter().map(|x| map!(x)).collect()),
        AsmE(inst, args) => AsmE(inst, args.into_iter().map(|x| map!(x)).collect()),
        Const(_) | ExtArray(_) => e.item,
    };

    e
}

pub type TyMap = util::Map<Id, ty::Ty>;

// α 変換されている前提で変数と型の対応を作る
pub fn make_tymap(e: &Expr, env: &mut TyMap) {
    macro_rules! push {
        ($decl: expr) => {
            env.insert($decl.name.clone(), $decl.t.clone().into())
        };
    }

    match &e.item {
        ExprKind::If(_, _, _, e1, e2) => {
            make_tymap(e1, env);
            make_tymap(e2, env);
        }
        ExprKind::Let(decl, e1, e2) => {
            push!(decl);
            make_tymap(e1, env);
            make_tymap(e2, env);
        }
        ExprKind::LetRec(fundef, e2) => {
            push!(fundef.fvar);
            for d in &fundef.args {
                push!(d);
            }

            make_tymap(&fundef.body, env);
            make_tymap(e2, env);
        }
        ExprKind::Loop { vars, body, .. } => {
            for d in vars {
                push!(d);
            }
            make_tymap(body, env);
        }
        _ => (),
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
            Var(v) => write!(f, "Var {}\n", v),
            ExtArray(x) => write!(f, "ExtArray {}\n", x),
            UnOp(op, x) => write!(f, "{:?} {}\n", op, x),
            BinOp(op, x, y) => write!(f, "{:?} {}, {}\n", op, x, y),
            If(kind, x, y, e1, e2) => {
                write!(f, "{:?} {}, {}:\n", kind, x, y)?;
                e1.item.format_indented(f, level + 1)?;
                write!(f, "{}Else:\n", indent(level))?;
                e2.item.format_indented(f, level + 1)
            }
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
            }
            Tuple(xs) => {
                write!(f, "Tuple ")?;
                util::format_vec(f, xs, "(", ", ", ")")?;
                write!(f, "\n")
            }
            App(func, args) => {
                write!(f, "App {func}")?;
                util::format_vec(f, args, "(", ", ", ")")?;
                write!(f, "\n")
            }
            ExtApp(func, args) => {
                write!(f, "ExtApp {func}")?;
                util::format_vec(f, args, "(", ", ", ")")?;
                write!(f, "\n")
            }
            CreateArray(num, init) => write!(f, "CreateArray {num}, {init}\n"),
            ArrayGet(arr, idx) => write!(f, "ArrayGet {arr}, {idx}\n"),
            ArrayPut(arr, idx, e) => write!(f, "ArrayPut {arr}, {idx}, {e}\n"),
            TupleGet(arr, idx) => write!(f, "TupleGet {arr}, {idx}\n"),
            Loop { vars, init, body } => {
                write!(f, "Loop:\n{}vars = ", indent(level))?;
                util::format_vec(f, vars, "[", ", ", "]")?;
                write!(f, "\n{}init = ", indent(level))?;
                util::format_vec(f, init, "[", ", ", "]")?;
                write!(f, "\n{}body =\n", indent(level))?;
                body.item.format_indented(f, level + 1)
            }
            Continue(xs) => {
                write!(f, "Continue ")?;
                util::format_vec(f, &xs.iter().map(|(_, x)| x).collect(), "[", ", ", "]")?;
                write!(f, "\n")
            }
            Asm(inst, args) => {
                write!(f, "Asm \"{inst}\"")?;
                util::format_vec(f, args, "(", ", ", ")")?;
                write!(f, "\n")
            }
            AsmE(inst, args) => {
                write!(f, "AsmE \"{inst}\"")?;
                util::format_vec(f, args, "(", ", ", ")")?;
                write!(f, "\n")
            }
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format_indented(f, 0)
    }
}
