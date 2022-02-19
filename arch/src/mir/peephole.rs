use super::mir::*;
use id_arena::Arena;
use util::{Id, Map};

fn conv(bid: BlockId, arena: &mut Arena<Block>) {
    let mut mul_cache: Map<Id, (Id, Id)> = Map::default();
    for (v, inst) in &mut arena[bid].body {
        if let Some(v) = v {
            mul_cache.remove(v);
        }
        match &inst.item {
            InstKind::FloatOp(FloatOpKind::FMul, x, y) => {
                mul_cache.insert(v.as_ref().unwrap().clone(), (x.clone(), y.clone()));
            }
            InstKind::FloatOp(FloatOpKind::FAdd, x, y) => {
                if let Some((z, w)) = mul_cache.get(x) {
                    inst.item = InstKind::FAddMul(z.clone(), w.clone(), y.clone());
                } else if let Some((z, w)) = mul_cache.get(y) {
                    inst.item = InstKind::FAddMul(z.clone(), w.clone(), x.clone());
                }
            }
            _ => (),
        }
    }
}

pub fn faddmul(mut p: Program) -> Program {
    for bid in p.collect_main_used() {
        conv(bid, &mut p.main_arena);
    }

    for f in &mut p.fundefs {
        for bid in f.collect_used() {
            conv(bid, &mut f.block_arena);
        }
    }

    p
}
