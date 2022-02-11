use crate::virt::program as virt;
use id_arena::Arena;

pub use ty::closure::Ty;
use util::{Id, Map, Spanned};

use std::fmt;

pub use virt::{FloatOpKind, IfKind, IntOpKind, Label, UnOpKind, Value};

// SSA の右辺
// 関数間解析をしないので ExtApp は CallDir に統合する
#[derive(Debug, Clone, PartialEq)]
pub enum InstKind {
    Nop,
    Mv(Id),
    Li(i32),
    FLi(f32),
    GetLabel(Label),
    LoadLabel(Label),
    UnOp(UnOpKind, Id),
    IntOp(IntOpKind, Id, Value),
    FloatOp(FloatOpKind, Id, Id),
    CallDir(Label, Vec<Id>),
    CallCls(Id, Vec<Id>),
    AllocHeap(Value),
    Lw(Id, Value),
    Sw(Id, Value, Id),
    In,
    Out(Id),
}

pub type Inst = Spanned<InstKind>;

impl fmt::Display for InstKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use InstKind::*;
        match self {
            Nop => write!(f, "Nop"),
            Mv(v) => write!(f, "Mv {v}"),
            Li(x) => write!(f, "Li {x}"),
            FLi(x) => write!(f, "FLi {x}"),
            GetLabel(label) => write!(f, "GetLabel {label}"),
            LoadLabel(label) => write!(f, "LoadLabel {label}"),
            UnOp(op, x) => write!(f, "{:?} {x}", op),
            IntOp(op, x, y) => write!(f, "{:?} {x}, {y}", op),
            FloatOp(op, x, y) => write!(f, "{:?} {x}, {y}", op),
            AllocHeap(x) => write!(f, "AllocHeap {x}"),
            CallDir(func, args) => {
                write!(f, "CallDir {func}")?;
                util::format_vec(f, args, "(", ", ", ")")
            }
            CallCls(func, args) => {
                write!(f, "CallCls {func}")?;
                util::format_vec(f, args, "(", ", ", ")")
            }
            Lw(x, y) => write!(f, "Lw {x}, {y}"),
            Sw(x, y, z) => write!(f, "Sw {x}, {y}, {z}"),
            In => write!(f, "In"),
            Out(x) => write!(f, "Out {x}"),
        }
    }
}

// basic block の末尾にあるもの
#[derive(Debug, Clone, PartialEq)]
pub enum TailKind {
    If(IfKind, Id, Value, BlockId, BlockId),
    IfF(IfKind, Id, Id, BlockId, BlockId),
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
                    "If {:?} {}, {} => {} | {}",
                    kind, x, y, arena[*b1].name, arena[*b2].name
                )
            }
            IfF(kind, x, y, b1, b2) => {
                write!(
                    f,
                    "IfF {:?} {}, {} => {} | {}",
                    kind, x, y, arena[*b1].name, arena[*b2].name
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
    pub exit: BlockId,
}

impl Program {
    pub fn new(block_arena: Arena<Block>, entry: BlockId, exit: BlockId) -> Self {
        Self {
            tymap: Map::default(),
            block_arena,
            globals: vec![],
            fundefs: vec![],
            entry,
            exit,
        }
    }

    fn collect_used_impl(&self, bid: BlockId, used: &mut util::Set<BlockId>) {
        if used.contains(&bid) {
            return;
        }
        used.insert(bid);

        match self.block_arena[bid].tail.item {
            TailKind::If(_, _, _, b1, b2) | TailKind::IfF(_, _, _, b1, b2) => {
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