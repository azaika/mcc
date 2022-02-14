use super::mir::*;
use id_arena::Arena;

type Set = util::Set<BlockId>;

fn conv(bid: BlockId, arena: &mut Arena<Block>, arrived: &mut Set) {
    if arrived.contains(&bid) {
        return;
    }
    arrived.insert(bid);

    let (mut add_insts, new_tail) = match arena[bid].tail.item {
        TailKind::Jump(b) => {
            conv(b, arena, arrived);
            let b = &arena[b];
            if b.body.len() >= 3 {
                return;
            }
            (b.body.clone(), b.tail.clone())
        }
        _ => return,
    };

    let b = &mut arena[bid];
    b.body.append(&mut add_insts);
    b.tail = new_tail;
}

pub fn merge_block(mut p: Program) -> Program {
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
