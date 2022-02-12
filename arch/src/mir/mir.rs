use crate::virt::program as virt;
use id_arena::Arena;

pub use ty::closure::Ty;
use util::{Id, Map, Spanned};

use std::fmt;

pub use virt::{FloatOpKind, IfKind, IntOpKind, Label, TyMap, UnOpKind, Value};

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
    CallDir(Label),
    CallCls,
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
            CallDir(func) => write!(f, "CallDir {func}"),
            CallCls => write!(f, "CallCls"),
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
    Return,
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
            Return => write!(f, "Return")
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
            tail: Box::new(Spanned::new(TailKind::Return, (0, 0))),
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

fn collect_used_impl(arena: &Arena<Block>, bid: BlockId, used: &mut util::Set<BlockId>) {
    if used.contains(&bid) {
        return;
    }
    used.insert(bid);

    match arena[bid].tail.item {
        TailKind::If(_, _, _, b1, b2) | TailKind::IfF(_, _, _, b1, b2) => {
            collect_used_impl(arena, b1, used);
            collect_used_impl(arena, b2, used);
        }
        TailKind::Jump(b) => collect_used_impl(arena, b, used),
        TailKind::Return => {}
    }
}

pub fn collect_used(arena: &Arena<Block>, entry: BlockId) -> util::Set<BlockId> {
    let mut used = util::Set::default();
    collect_used_impl(arena, entry, &mut used);
    used
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fundef {
    pub name: Label,
    pub args: Vec<Id>,
    pub formal_fv: Vec<Id>,
    pub entry: BlockId,
    pub block_arena: Arena<Block>,
}

impl Fundef {
    pub fn collect_used(&self) -> util::Set<BlockId> {
        collect_used(&self.block_arena, self.entry)
    }

    fn format_indented(
        &self,
        f: &mut fmt::Formatter,
        tymap: &Map<Id, Ty>,
        level: usize,
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
            self.block_arena[self.entry].name
        )?;

        write!(f, "{}blocks:\n", indent(level + 1))?;
        for bid in &self.collect_used() {
            self.block_arena[*bid].format_indented(f, level + 2, &self.block_arena)?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub tymap: TyMap,
    pub main_arena: Arena<Block>,
    pub globals: Vec<(Label, usize)>,
    pub fundefs: Vec<Fundef>,
    pub entry: BlockId,
    pub exit: BlockId,
}

impl Program {
    pub fn new(block_arena: Arena<Block>, entry: BlockId, exit: BlockId) -> Self {
        Self {
            tymap: Map::default(),
            main_arena: block_arena,
            globals: vec![],
            fundefs: vec![],
            entry,
            exit,
        }
    }
    pub fn collect_main_used(&self) -> util::Set<BlockId> {
        collect_used(&self.main_arena, self.entry)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Variables: [\n")?;
        for (x, t) in &self.tymap {
            write!(f, "    {x}: {t},\n")?;
        }
        write!(f, "]\n\n")?;

        write!(f, "Entry: {}\n\n", self.main_arena[self.entry].name)?;

        write!(f, "Globals: [\n")?;
        for (label, size) in &self.globals {
            write!(f, "    ({label}, {size}),\n")?;
        }

        write!(f, "]\n\nFundefs:\n")?;
        for fun in &self.fundefs {
            fun.format_indented(f, &self.tymap, 1)?;
            write!(f, "\n")?;
        }

        write!(f, "Blocks:\n")?;
        for bid in &self.collect_main_used() {
            self.main_arena[*bid].format_indented(f, 1, &self.main_arena)?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}
