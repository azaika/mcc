use id_arena::Arena;

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
pub enum TailKind {
    If(IfKind, Id, Id, BlockId, BlockId),
    ForEach(Id, Id, BlockId), // (element, array_var, body)
    Jump(BlockId),
    Return(Id),
}

impl TailKind {
    fn format(&self, f: &mut fmt::Formatter, arena: &Arena<Block>) -> fmt::Result {
        use TailKind::*;
        match self {
            If(kind, x, y, b1, b2) => {
                write!(f, "{:?} {}, {} => {} | {}", kind, x, y, arena[*b1].name, arena[*b2].name)
            },
            ForEach(e, arr, body) => {
                write!(f, "ForEach {} <- {} => {}", e, arr, arena[*body].name)
            },
            Jump(block) => write!(f, "Jump {}", arena[*block].name),
            Return(r) => write!(f, "Return {}", r),
        }
    }
}

pub type Tail = Spanned<TailKind>;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub name: String,
    pub body: Vec<(Id, Inst)>,
    pub tail: Box<Tail>
}

impl Block {
    fn format_indented(&self, f: &mut fmt::Formatter, level: usize, arena: &Arena<Block>) -> fmt::Result {
        // print indentation
        let indent = |level: usize| "    ".repeat(level);
        write!(f, "{}Block {}\n", indent(level), self.name)?;
        write!(f, "{}body:\n", indent(level))?;
        for (x, inst) in &self.body {
            write!(f, "{}{} <- {}\n", indent(level + 1), x, inst)?;
        }
        write!(f, "{}tail: ", indent(level + 1))?;
        self.tail.item.format(f, arena)?;
        write!(f, "\n")
    }
}

pub type BlockId = id_arena::Id<Block>;

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    pub name: Label,
    pub args: Vec<Id>,
    pub formal_fv: Vec<Id>,
    pub entry: BlockId
}

impl Fundef {
    fn format_indented(&self, f: &mut fmt::Formatter, tymap: &Map<Id, Ty>, level: usize, arena: &Arena<Block>) -> fmt::Result {
        // print indentation
        let indent = |level: usize| "    ".repeat(level);
        write!(f, "{}Fundef ({}, {})\n", indent(level), self.name, tymap.get(&self.name.0).unwrap())?;
        write!(f, "{}args: ", indent(level + 1))?;
        util::format_vec(f, &self.args, "[", ", ", "]")?;
        write!(f, "\n{}formal_fv: ", indent(level + 1))?;
        util::format_vec(f, &self.formal_fv, "[", ", ", "]")?;
        write!(f, "\n{}entry: {}\n", indent(level + 1), arena[self.entry].name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
    pub name: Label,
    pub init: BlockId
}

impl Global {
    fn format(&self, f: &mut fmt::Formatter, arena: &Arena<Block>) -> fmt::Result {
        write!(f, "{} => {}", self.name, arena[self.init].name)
    }
}

pub struct Program {
    pub tymap: Map<Id, Ty>,
    pub block_arena: Arena<Block>,
    pub globals: Vec<Global>,
    pub fundefs: Vec<Fundef>,
    pub entry: BlockId
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Entry: {}\n\n", self.block_arena[self.entry].name)?;

        write!(f, "Globals:\n")?;
        for global in &self.globals {
            write!(f, "    ")?;
            global.format(f, &self.block_arena)?;
            write!(f, "\n")?;
        }

        write!(f, "Fundefs:\n")?;
        for fun in &self.fundefs {
            fun.format_indented(f, &self.tymap, 1, &self.block_arena)?;
            write!(f, "\n")?;
        }

        write!(f, "Blocks:\n")?;
        for (_, block) in &self.block_arena {
            block.format_indented(f, 1, &self.block_arena)?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}