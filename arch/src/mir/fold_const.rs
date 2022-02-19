use super::mir::*;
use id_arena::Arena;
use util::{Id, Map, Set};

fn is_16bit(x: i32) -> bool {
    i16::MIN as i32 <= x && x <= i16::MAX as i32
}

fn to_vi(v: &Value, consts: &Map<Id, i32>) -> Option<i32> {
    match v {
        Value::Var(y) => consts.get(y).cloned(),
        Value::Imm(x) => Some(*x as i32),
    }
}

fn conv(bid: BlockId, arena: &mut Arena<Block>, arrived: &mut Set<BlockId>) {
    if arrived.contains(&bid) {
        return;
    }
    arrived.insert(bid);

    let mut const_i: Map<Id, i32> = Map::default();
    let mut const_f: Map<Id, f32> = Map::default();
    for (v, inst) in &mut arena[bid].body {
        if let Some(v) = v {
            const_i.remove(v);
            const_f.remove(v);
        }
        match &inst.item {
            InstKind::Mv(x) => {
                if let Some(x) = const_i.get(x).cloned() {
                    const_i.insert(v.as_ref().unwrap().clone(), x);
                    if is_16bit(x) {
                        inst.item = InstKind::Li(x);
                    }
                } else if let Some(x) = const_f.get(x).cloned() {
                    const_f.insert(v.as_ref().unwrap().clone(), x);
                }
            }
            InstKind::Li(x) => {
                const_i.insert(v.as_ref().unwrap().clone(), *x);
            }
            InstKind::FLi(x) => {
                const_f.insert(v.as_ref().unwrap().clone(), *x);
            }
            _ => (),
        }
    }

    let tail = &mut arena[bid].tail;
    match &tail.item {
        TailKind::If(kind, x, y, b1, b2) => {
            if let Some(x) = const_i.get(x).cloned() {
                if let Some(y) = to_vi(y, &const_i) {
                    let cond = match kind {
                        IfKind::IfEq => x == y,
                        IfKind::IfLE => x <= y,
                        IfKind::IfGE => x >= y,
                    };

                    if cond {
                        let b1 = *b1;
                        tail.item = TailKind::Jump(b1);
                    } else {
                        let b2 = *b2;
                        tail.item = TailKind::Jump(b2);
                    }
                }
            }
        }
        TailKind::IfF(kind, x, y, b1, b2) => {
            if let Some(x) = const_f.get(x).cloned() {
                if let Some(y) = const_f.get(y).cloned() {
                    let cond = match kind {
                        IfKind::IfEq => x == y,
                        IfKind::IfLE => x <= y,
                        IfKind::IfGE => x >= y,
                    };

                    if cond {
                        let b1 = *b1;
                        tail.item = TailKind::Jump(b1);
                    } else {
                        let b2 = *b2;
                        tail.item = TailKind::Jump(b2);
                    }
                }
            }
        }
        _ => (),
    }
}

pub fn fold_const(mut p: Program) -> Program {
    let mut arrived = Set::default();
    arrived.insert(p.exit);
    for bid in p.collect_main_used() {
        conv(bid, &mut p.main_arena, &mut arrived);
    }

    for f in &mut p.fundefs {
        arrived.clear();
        for bid in f.collect_used() {
            conv(bid, &mut f.block_arena, &mut arrived);
        }
    }

    p
}
