use std::{fmt, hash::Hash};

use util::{Spanned, Id};
use ty::knormal as ty;

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

impl Hash for ConstKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            ConstKind::CInt(i) => i.hash(state),
            ConstKind::CFloat(d) => d.to_bits().hash(state),
            _ => ()
        }
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

#[derive(Debug, Clone, PartialEq, Hash)]
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

impl LetKind {
    pub fn map<F>(self, mut f: F) -> Self
        where F: FnMut(Box<Expr>) -> Box<Expr>
    {
        match self {
            LetKind::Let(d, e1, e2) => LetKind::Let(d, f(e1), f(e2)),
            LetKind::LetRec(Fundef { fvar, args, body }, e2) => {
                let body = f(body);
                let e2 = f(e2);

                LetKind::LetRec(Fundef { fvar, args, body }, e2)
            },
            LetKind::LetTuple(ds, x, e2) => LetKind::LetTuple(ds, x, f(e2)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
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

type Map = util::Map<Id, Id>;

// `e` must be alpha formed
pub fn rename(mut e: Box<Expr>, env: &Map) -> Box<Expr> {
    macro_rules! map {
        ($name: expr) => {
            if let Some(_x) = env.get(&$name) { _x.clone() } else { $name.clone() }
        }
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
        },
        Let(l) => {
            let kind = match l {
                LetKind::Let(decl, e1, e2) => {
                    let e1 = rename(e1, env);
                    let e2 = rename(e2, env);
                    LetKind::Let(Decl::new(map!(decl.name), decl.t), e1, e2)
                },
                LetKind::LetRec(fundef, e2) => {
                    let decl = fundef.fvar;
                    let body =  rename(fundef.body, env);
                    let args = fundef.args.into_iter().map(|d| Decl::new(map!(d.name), d.t)).collect();
                    let e2 =  rename(e2, env);

                    let fundef = Fundef {
                        fvar: Decl::new(map!(decl.name), decl.t),
                        args,
                        body
                    };
                    LetKind::LetRec(fundef, e2)
                },
                LetKind::LetTuple(ds, x, e2) => {
                    let ds = ds.into_iter().map(|d| Decl::new(map!(d.name), d.t)).collect();
                    LetKind::LetTuple(ds, map!(x), rename(e2, env))
                }
            };

            Let(kind)
        },
        Tuple(xs) => Tuple(xs.into_iter().map(|x| map!(x)).collect()),
        App(f, args) => App(map!(f), args.into_iter().map(|x| map!(x)).collect()),
        ExtApp(f, args) => ExtApp(f, args.into_iter().map(|x| map!(x)).collect()),
        CreateArray(num, init) => CreateArray(map!(num), map!(init)),
        Get(x, y) => Get(map!(x), map!(y)),
        Put(x, y, z) => Put(map!(x), map!(y), map!(z)),
        _ => e.item
    };

    e
}

impl ExprKind {
    fn hash_impl<H: std::hash::Hasher>(&self, state: &mut H, num_let: usize) {
        core::mem::discriminant(self).hash(state);
        use ExprKind::*;
        match self {
            Const(c) => c.hash(state),
            Var(x) | ExtArray(x) => x.hash(state),
            UnOp(op, x) => {op.hash(state); x.hash(state)},
            BinOp(op, x, y) => {
                op.hash(state);
                use BinOpKind::*;
                let (x, y) = match op {
                    Add | FAdd | Mul | FMul if x > y => (y, x),
                    _ => (x, y),
                };

                x.hash(state);
                y.hash(state)
            },
            If(kind, x, y, e1, e2) => {
                kind.hash(state);
                let (x, y) = if x < y { (x, y) } else { (y, x) };
                x.hash(state);
                y.hash(state);
                e1.item.hash_impl(state, num_let);
                e2.item.hash_impl(state, num_let)
            },
            Let(l) => {
                match l {
                    LetKind::Let(d, e1, e2) => {
                        e1.item.hash_impl(state, num_let);

                        let new_name = format!("V{}{}", ty::short(&d.t), num_let);

                        new_name.hash(state);

                        let mut m = Map::default();
                        m.insert(d.name.clone(), new_name);

                        let e2 = rename(Box::new(e2.as_ref().clone()), &m);

                        e2.item.hash_impl(state, num_let + 1)
                    },
                    LetKind::LetRec(Fundef { fvar, args, body }, e2) => {
                        fvar.hash(state);
                        args.hash(state);
                        body.item.hash_impl(state, num_let);
                        e2.item.hash_impl(state, num_let);
                    },
                    LetKind::LetTuple(ds, x, e2) => {
                        ds.hash(state);
                        x.hash(state);
                        e2.item.hash_impl(state, num_let);
                    },
                }
            },
            Tuple(xs) => xs.hash(state),
            App(f, args) | ExtApp(f, args) => {
                f.hash(state);
                args.hash(state)
            },
            CreateArray(x, y) | Get(x, y) => {
                x.hash(state);
                y.hash(state)
            },
            Put(x, y, z) => {
                x.hash(state);
                y.hash(state);
                z.hash(state)
            },
        }
    }

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

impl Hash for ExprKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash_impl(state, 0);
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format_indented(f, 0)
    }
}