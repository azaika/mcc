use id_arena::Arena;

pub use ty::mir::Ty;
use util::{Id, Map, Spanned};

use std::fmt;

#[derive(Debug, Clone, PartialEq, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnOpKind {
    Neg,
    FNeg,
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
    FDiv,
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
    AllocArray(Id, Ty),
    Assign(Label, Id),
    Load(Label),
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
            Var(v) => write!(f, "Var {v}"),
            ExtArray(x) => write!(f, "ExtArray {x}"),
            UnOp(op, x) => write!(f, "{:?} {x}", op),
            BinOp(op, x, y) => write!(f, "{:?} {x}, {y}", op),
            Tuple(xs) => {
                write!(f, "Tuple ")?;
                util::format_vec(f, xs, "(", ", ", ")")
            }
            CallDir(lab, args) => {
                write!(f, "CallDir {lab}")?;
                util::format_vec(f, args, "(", ", ", ")")
            }
            CallCls(func, args) => {
                write!(f, "CallCls {func}")?;
                util::format_vec(f, args, "(", ", ", ")")
            }
            AllocArray(num, t) => write!(f, "AllocArray<{t}>({num})"),
            Assign(x, y) => write!(f, "Assign {x} := {y}"),
            Load(x) => write!(f, "Load {x}"),
            ArrayGet(arr, idx) => write!(f, "ArrayGet {arr}[{idx}]"),
            ArrayPut(arr, idx, e) => write!(f, "ArrayPut {arr}[{idx}] <- {e}"),
            TupleGet(arr, idx) => write!(f, "TupleGet {arr}.{idx}"),
            MakeCls(lab, actual_fv) => {
                write!(f, "MakeCls {lab}")?;
                util::format_vec(f, actual_fv, "[", ", ", "]")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IfKind {
    IfEq,
    IfLE,
}

// basic block の末尾にあるもの
#[derive(Debug, Clone, PartialEq)]
pub enum TailKind {
    If(IfKind, Id, Id, BlockId, BlockId),
    ForEach(Id, Id, Option<Id>, BlockId, BlockId), // (idx, array_var, size?, body, cont)
    Jump(BlockId),
    Return(Option<Id>),
}

impl TailKind {
    fn format(&self, f: &mut fmt::Formatter, arena: &Arena<Block>) -> fmt::Result {
        use TailKind::*;
        match self {
            If(kind, x, y, b1, b2) => {
                write!(
                    f,
                    "{:?} {}, {} => {} | {}",
                    kind, x, y, arena[*b1].name, arena[*b2].name
                )
            }
            ForEach(idx, arr, _, body, cont) => {
                write!(
                    f,
                    "ForEach {}[{}] => {} ? {}",
                    arr, idx, arena[*body].name, arena[*cont].name
                )
            }
            Jump(block) => write!(f, "Jump {}", arena[*block].name),
            Return(r) => {
                if let Some(r) = r {
                    write!(f, "Return {}", r)
                } else {
                    write!(f, "Return")
                }
            }
        }
    }
}

pub type Tail = Spanned<TailKind>;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub name: String,
    pub body: Vec<(Option<Id>, Inst)>,
    pub tail: Box<Tail>,
}

impl Block {
    pub fn new() -> Self {
        Self::with_name(util::id::gen_uniq_with(".b"))
    }
    pub fn with_name(name: String) -> Self {
        Self {
            name,
            body: vec![],
            tail: Box::new(Spanned::new(TailKind::Return(None), (0, 0))),
        }
    }

    fn format_indented(
        &self,
        f: &mut fmt::Formatter,
        level: usize,
        arena: &Arena<Block>,
    ) -> fmt::Result {
        // print indentation
        let indent = |level: usize| "    ".repeat(level);
        write!(f, "{}Block {}", indent(level), self.name)?;
        write!(f, "\n{}body:\n", indent(level + 1))?;
        for (x, inst) in &self.body {
            if let Some(x) = x {
                write!(f, "{}{} <- {}\n", indent(level + 2), x, inst)?;
            } else {
                write!(f, "{}{}\n", indent(level + 2), inst)?;
            }
        }

        write!(f, "\n{}tail: ", indent(level + 1))?;
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
    pub entry: BlockId,
}

impl Fundef {
    fn format_indented(
        &self,
        f: &mut fmt::Formatter,
        tymap: &Map<Id, Ty>,
        level: usize,
        arena: &Arena<Block>,
    ) -> fmt::Result {
        // print indentation
        let indent = |level: usize| "    ".repeat(level);
        write!(
            f,
            "{}Fundef ({}, {})\n",
            indent(level),
            self.name,
            tymap.get(&self.name.0).unwrap()
        )?;
        write!(f, "{}args: ", indent(level + 1))?;
        util::format_vec(f, &self.args, "[", ", ", "]")?;
        write!(f, "\n{}formal_fv: ", indent(level + 1))?;
        util::format_vec(f, &self.formal_fv, "[", ", ", "]")?;
        write!(
            f,
            "\n{}entry: {}\n",
            indent(level + 1),
            arena[self.entry].name
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub tymap: Map<Id, Ty>,
    pub block_arena: Arena<Block>,
    pub globals: Vec<Label>,
    pub fundefs: Vec<Fundef>,
    pub entry: BlockId,
}

impl Program {
    pub fn new(block_arena: Arena<Block>, entry: BlockId) -> Self {
        Self {
            tymap: Map::default(),
            block_arena,
            globals: vec![],
            fundefs: vec![],
            entry,
        }
    }

    fn collect_used_impl(&self, bid: BlockId, used: &mut util::Set<BlockId>) {
        if used.contains(&bid) {
            return;
        }
        used.insert(bid);

        match self.block_arena[bid].tail.item {
            TailKind::If(_, _, _, b1, b2) | TailKind::ForEach(_, _, _, b1, b2) => {
                self.collect_used_impl(b1, used);
                self.collect_used_impl(b2, used);
            }
            TailKind::Jump(b) => self.collect_used_impl(b, used),
            TailKind::Return(_) => {}
        }
    }

    pub fn collect_used(&self) -> util::Set<BlockId> {
        let mut used = util::Set::default();

        self.collect_used_impl(self.entry, &mut used);
        for f in &self.fundefs {
            self.collect_used_impl(f.entry, &mut used);
        }

        used
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Entry: {}\n\n", self.block_arena[self.entry].name)?;

        write!(f, "Globals: [\n")?;
        for g in &self.globals {
            write!(f, "    {},\n", g)?;
        }

        write!(f, "]\n\nFundefs:\n")?;
        for fun in &self.fundefs {
            fun.format_indented(f, &self.tymap, 1, &self.block_arena)?;
            write!(f, "\n")?;
        }

        write!(f, "Blocks:\n")?;
        for bid in &self.collect_used() {
            self.block_arena[*bid].format_indented(f, 1, &self.block_arena)?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}
