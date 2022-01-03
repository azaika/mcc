use typed_arena::Arena;
use util::{Spanned, Id, Map};
pub use ty::closure::Ty as Ty;

use std::{fmt, cell::RefCell};

#[derive(Debug, Clone, PartialEq)]
pub struct Label(pub Id);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ".L({})", self.0)
    }
}

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnOpKind {
    Neg,
    FNeg
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

// SSA の右辺
// 関数間解析をしないので ExtApp は CallDir に統合する
#[derive(Debug, Clone, PartialEq)]
pub enum InstKind {
    Const(ConstKind),
    Var(Id),
    UnOp(UnOpKind, Id),
    BinOp(BinOpKind, Id, Id),
    Tuple(Vec<Id>),
    CallDir(Label, Vec<Id>),
    CallCls(Id, Vec<Id>),
    CreateArray(Id, Id),
    ExtArray(Label),
    TupleGet(Id, usize),
    ArrayGet(Id, Id),
    ArrayPut(Id, Id, Id),
    MakeCls(Label, Vec<Id>), // (label, actual_fv)
}

pub type Inst = Spanned<InstKind>;

impl fmt::Display for InstKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use InstKind::*;
        match self {
            Const(c) => write!(f, "{:?}", c),
            Var(v) => write!(f, "Var {}", v),
            ExtArray(x) => write!(f, "ExtArray {}", x),
            UnOp(op, x) => write!(f, "{:?} {}", op, x),
            BinOp(op, x, y) => write!(f, "{:?} {}, {}", op, x, y),
            Tuple(xs) => {
                write!(f, "Tuple ")?;
                util::format_vec(f, xs, "(", ", ", ")")
            },
            CallDir(lab, args) => {
                write!(f, "CallDir {}", lab)?;
                util::format_vec(f, args, "(", ", ", ")")
            },
            CallCls(func, args) => {
                write!(f, "CallCls {}", func)?;
                util::format_vec(f, args, "(", ", ", ")")
            },
            CreateArray(num, init) => write!(f, "CreateArray {}, {}", num, init),
            ArrayGet(arr, idx) => write!(f, "ArrayGet {}[{}]", arr, idx),
            ArrayPut(arr, idx, e) => write!(f, "ArrayPut {}[{}] <- {}", arr, idx, e),
            TupleGet(arr, idx) => write!(f, "TupleGet {}.{}", arr, idx),
            MakeCls(lab, actual_fv) => {
                write!(f, "MakeCls {}", lab)?;
                util::format_vec(f, actual_fv, "[", ", ", "]")
            },
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IfKind {
    IfEq,
    IfLE
}

// basic block の末尾にあるもの
#[derive(Debug, Clone, PartialEq)]
pub enum TailKind<'a> {
    If(IfKind, Id, Id, RefCell<Block<'a>>, RefCell<Block<'a>>),
    ForEach(Id, Id, RefCell<Block<'a>>), // (element, array_var, body)
    Jump(RefCell<Block<'a>>),
    Return(Id),
}

impl<'a> fmt::Display for TailKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TailKind::*;
        match self {
            If(kind, x, y, e1, e2) => {
                write!(f, "{:?} {}, {} => {} | {}", kind, x, y, e1.borrow().name, e2.borrow().name)
            },
            ForEach(e, arr, body) => {
                write!(f, "ForEach {} <- {} => {}", e, arr, body.borrow().name)
            },
            Jump(block) => write!(f, "Jump {}", block.borrow().name),
            Return(r) => write!(f, "Return {}", r),
        }
    }
}

pub type Tail<'a> = Spanned<TailKind<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub struct BlockData<'a> {
    pub name: String,
    pub body: Vec<(Id, Inst)>,
    pub tail: Box<Tail<'a>>
}

pub type Block<'a> = &'a BlockData<'a>;

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef<'a> {
    pub name: Label,
    pub args: Vec<Id>,
    pub formal_fv: Vec<Id>,
    pub entry: RefCell<Block<'a>>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global<'a> {
    pub name: Label,
    pub init: RefCell<Block<'a>>
}

pub struct Program<'a> {
    pub tymap: Map<Id, Ty>,
    pub block_arena: Arena<Block<'a>>,
    pub globals: Vec<Global<'a>>,
    pub fundefs: Vec<Fundef<'a>>,
    pub entry: RefCell<Block<'a>>
}
