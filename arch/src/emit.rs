use crate::common::*;
use crate::mir::mir::*;
use crate::regalloc::RegMap;
use id_arena::Arena;
use std::io::{Result, Write};
use util::{Id, Map, Set};

fn emit_inst<W: Write>(
    w: &mut W,
    v: &Option<Id>,
    inst: &Inst,
    regmap: &RegMap,
    stackmap: &mut Map<Id, i32>,
    stack_size: &mut i32,
) -> Result<()> {
    use InstKind::*;
    macro_rules! reg_v {
        () => {
            regmap.get(v.as_ref().unwrap()).unwrap()
        };
    }
    macro_rules! reg {
        ($x: ident) => {
            regmap.get($x).unwrap()
        };
    }
    match &inst.item {
        Nop => return Ok(()),
        Mv(x) => {
            let v = reg_v!();
            let x = reg!(x);
            if x == v {
                return Ok(());
            }

            write!(w, "\tmv\t\t{v}, {x}")?
        }
        Li(i) => {
            if i16::MIN as i32 <= *i && *i <= i16::MAX as i32 {
                write!(w, "\taddi\t{}, {REG_ZERO}, {i}", reg_v!())?;
            } else {
                let v = reg_v!();
                let i = *i as u32;
                let lo = (i << 16) >> 16;
                let ha = (i ^ lo) >> 16;
                write!(w, "\taddis\t{v}, {REG_ZERO}, {ha}\n")?;
                write!(w, "\tori\t\t{v}, {v}, {lo}")?;
            }
        }
        FLi(x) => {
            let v = reg_v!();
            let x = x.to_bits();
            let lo = (x << 16) >> 16;
            let ha = (x ^ lo) >> 16;
            write!(w, "\taddis\t{v}, {REG_ZERO}, {ha}\n")?;
            write!(w, "\tori\t\t{v}, {v}, {lo}")?
        }
        GetLabel(label) => {
            let v = reg_v!();
            write!(w, "\taddis\t{v}, {REG_ZERO}, ha16({})\n", label.0)?;
            write!(w, "\tori\t\t{v}, {v}, lo16({})", label.0)?
        }
        LoadLabel(label) => write!(w, "\tlwi\t\t{}, {}", reg_v!(), label.0)?,
        UnOp(kind, x) => {
            let v = reg_v!();
            let f = match kind {
                UnOpKind::Neg => {
                    return write!(
                        w,
                        "\tsub\t\t{v}, {REG_ZERO}, {}\t\t\t\t# {}\n",
                        reg!(x),
                        inst.loc.0
                    );
                }
                UnOpKind::FNeg => "fneg",
                UnOpKind::FAbs => "fabs",
                UnOpKind::FSin => "fsin",
                UnOpKind::FCos => "fcos",
                UnOpKind::FAtan => "fatan",
                UnOpKind::FSqrt => "fsqrt",
                UnOpKind::Ftoi => "ftoi",
                UnOpKind::Itof => "itof",
                UnOpKind::FFloor => "ffloor",
                UnOpKind::FHalf => "fhalf",
            };
            write!(w, "\t{f}\t{v}, {}", reg!(x))?
        }
        IntOp(kind, x, Value::Imm(y)) => {
            let f = match kind {
                IntOpKind::Add => "addi",
                IntOpKind::Sub => {
                    assert!(*y != i16::MIN);
                    return write!(
                        w,
                        "\taddi\t{}, {}, {}\t\t\t\t# {}\n",
                        reg_v!(),
                        reg!(x),
                        (-y) as i16,
                        inst.loc.0
                    );
                }
                IntOpKind::Mul16 => "mul16i",
                IntOpKind::Shl => {
                    assert!(0 <= *y && *y <= 31);
                    "shli"
                }
                IntOpKind::Shr => {
                    assert!(0 <= *y && *y <= 31);
                    "shri"
                }
            };
            write!(w, "\t{f}\t{}, {}, {y}", reg_v!(), reg!(x))?
        }
        IntOp(kind, x, Value::Var(y)) => {
            let f = match kind {
                IntOpKind::Add => "add\t",
                IntOpKind::Sub => "sub\t",
                IntOpKind::Mul16 => "mul16",
                IntOpKind::Shl | IntOpKind::Shr => panic!(),
            };
            write!(w, "\t{f}\t{}, {}, {}", reg_v!(), reg!(x), reg!(y))?
        }
        FloatOp(kind, x, y) => {
            let f = match kind {
                FloatOpKind::FAdd => "fadd",
                FloatOpKind::FSub => "fsub",
                FloatOpKind::FMul => "fmul",
                FloatOpKind::FDiv => "fdiv",
            };
            write!(w, "\t{f}\t{}, {}, {}", reg_v!(), reg!(x), reg!(y))?
        }
        CallDir(label) => {
            let stack_size = *stack_size;
            write!(w, "\tmflr\t{REG_TMP}\n")?;
            write!(w, "\tsw\t\t{REG_TMP}, {}({REG_STACK})\n", -stack_size)?;
            write!(
                w,
                "\taddi\t{REG_STACK}, {REG_STACK}, {}\n",
                -(stack_size + 1)
            )?;
            write!(w, "\tbl\t\t{}\n", label.0)?;
            write!(w, "\taddi\t{REG_STACK}, {REG_STACK}, {}\n", stack_size + 1)?;
            write!(w, "\tlw\t\t{REG_TMP}, {}({REG_STACK})\n", -stack_size)?;
            write!(w, "\tmtlr\t{REG_TMP}")?
        }
        CallCls => unimplemented!(),
        AllocHeap(s) => {
            write!(w, "\tmv\t\t{}, {REG_HEAP}\n", reg_v!())?;
            match s {
                Value::Var(s) => write!(w, "\tadd\t\t{REG_HEAP}, {REG_HEAP}, {}", reg!(s))?,
                Value::Imm(s) => write!(w, "\taddi\t{REG_HEAP}, {REG_HEAP}, {s}")?,
            }
        }
        Lw(x, Value::Imm(y)) => write!(w, "\tlw\t\t{}, {y}({})", reg_v!(), reg!(x))?,
        Lw(x, Value::Var(y)) => write!(w, "\tlwx\t\t{}, {}, {}", reg_v!(), reg!(x), reg!(y))?,
        Sw(x, Value::Imm(y), z) => write!(w, "\tsw\t\t{}, {y}({})", reg!(z), reg!(x))?,
        Sw(x, Value::Var(y), z) => write!(w, "\tswx\t\t{}, {}, {}", reg!(z), reg!(y), reg!(x))?,
        In => write!(w, "\tin")?,
        Out(x) => write!(w, "\tout\t\t{}", reg!(x))?,
        Save(tag, x) => {
            if let Some(offset) = stackmap.get(tag) {
                write!(w, "\tsw\t\t{}, {offset}({REG_STACK})", reg!(x))?;
            } else {
                stackmap.insert(tag.clone(), *stack_size);
                write!(w, "\tsw\t\t{}, {}({REG_STACK})", reg!(x), -*stack_size)?;
                *stack_size += 1;
            }
        }
        Restore(tag) => {
            let offset = -stackmap.get(tag).unwrap();
            write!(w, "\tlw\t\t{}, {offset}({REG_STACK})", reg_v!())?;
        }
    }

    write!(w, "\t\t\t\t# {}\n", inst.loc.0)
}

fn emit_block<W: Write>(
    w: &mut W,
    bid: BlockId,
    arena: &Arena<Block>,
    arrived: &mut Set<BlockId>,
    regmap: &RegMap,
    stackmap: &mut Map<Id, i32>,
    stack_size: &mut i32,
) -> Result<()> {
    if arrived.contains(&bid) {
        return Ok(());
    }
    arrived.insert(bid);

    let block = &arena[bid];
    write!(w, "{}:\n", block.name)?;
    for (v, inst) in &block.body {
        emit_inst(w, v, inst, &regmap, stackmap, stack_size)?;
    }

    macro_rules! reg {
        ($x: ident) => {
            regmap.get($x).unwrap()
        };
    }
    match &block.tail.item {
        TailKind::If(kind, x, y, b1, b2) => {
            match y {
                Value::Var(y) => write!(w, "\tcmp\t\t{}, {}\n", reg!(x), reg!(y))?,
                Value::Imm(y) => write!(w, "\tcmpi\t{}, {y}\n", reg!(x))?,
            }
            let fb = match kind {
                IfKind::IfEq => "beq\t",
                IfKind::IfLE => "ble\t",
                IfKind::IfGE => "bge\t",
            };

            write!(w, "\t{fb}\t{}", arena[*b1].name)?;
            write!(w, "\t\t\t\t# {}\n", block.tail.loc.0)?;
            if arrived.contains(b2) {
                write!(w, "\tb\t\t{}\n", arena[*b2].name)?;
            }
            let original_ss = *stack_size;
            emit_block(w, *b2, arena, arrived, regmap, stackmap, stack_size)?;
            *stack_size = original_ss;
            emit_block(w, *b1, arena, arrived, regmap, stackmap, stack_size)?;
        }
        TailKind::IfF(kind, x, y, b1, b2) => {
            write!(w, "\tfcmp\t{}, {}\n", reg!(x), reg!(y))?;
            let fb = match kind {
                IfKind::IfEq => "beq\t",
                IfKind::IfLE => "ble\t",
                IfKind::IfGE => "bge\t",
            };

            write!(w, "\t{fb}\t{}", arena[*b1].name)?;
            write!(w, "\t\t\t\t# {}\n", block.tail.loc.0)?;
            if arrived.contains(b2) {
                write!(w, "\tb\t\t{}\n", arena[*b2].name)?;
            }
            let original_ss = *stack_size;
            emit_block(w, *b2, arena, arrived, regmap, stackmap, stack_size)?;
            *stack_size = original_ss;
            emit_block(w, *b1, arena, arrived, regmap, stackmap, stack_size)?;
        }
        TailKind::Jump(b1) => {
            if arrived.contains(b1) {
                write!(w, "\tb\t\t{}", arena[*b1].name)?;
                write!(w, "\t\t\t\t# {}\n", block.tail.loc.0)?;
            }
            emit_block(w, *b1, arena, arrived, regmap, stackmap, stack_size)?;
        }
        TailKind::Return => {
            write!(w, "\tblr")?;
            write!(w, "\t\t\t\t# {}\n", block.tail.loc.0)?;
        }
    }

    Ok(())
}

pub fn emit<W: Write>(w: &mut W, p: Program, regmaps: (RegMap, Map<Label, RegMap>)) -> Result<()> {
    write!(w, "\t.text\n")?;
    write!(w, "\tsub\t\t{REG_ZERO}, {REG_ZERO}, {REG_ZERO}\n")?;
    write!(w, "\tb\t\t_min_caml_start\n")?;

    for Fundef {
        name,
        entry,
        block_arena,
    } in &p.fundefs
    {
        write!(w, "\t# {}\n", name.0)?;
        write!(w, "{}:\n", name.0)?;
        emit_block(
            w,
            *entry,
            block_arena,
            &mut Set::default(),
            regmaps.1.get(&name).unwrap(),
            &mut Map::default(),
            &mut 0,
        )?;
    }

    write!(w, "\t# main program start\n")?;

    let mut arrived = Set::default();
    arrived.insert(p.exit);
    emit_block(
        w,
        p.entry,
        &p.main_arena,
        &mut arrived,
        &regmaps.0,
        &mut Map::default(),
        &mut 0,
    )?;

    write!(w, "\t# main program end\n")?;
    write!(w, "_min_caml_end:\n")?;
    write!(w, "\tflush\n")?;
    write!(w, "\thalt\n")?;
    write!(w, "\thalt\n")?;
    write!(w, "\thalt\n")?;
    write!(w, "# === .data section ===\n")?;
    write!(w, "\t.data\n")?;
    for (label, size) in p.globals {
        write!(w, "{}:\n", label.0)?;
        write!(w, "\t.size {size}\n")?;
    }
    Ok(())
}
