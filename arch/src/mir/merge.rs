use super::mir::*;

type Set = util::Set<BlockId>;

fn conv(bid: BlockId, p: &mut Program, arrived: &mut Set) {
    if arrived.contains(&bid) {
        return;
    }
    arrived.insert(bid);

    let (mut add_insts, new_tail) = match p.block_arena[bid].tail.item {
        TailKind::Jump(b) => {
            conv(b, p, arrived);
            let b = &p.block_arena[b];
            if b.body.len() >= 3 {
                return;
            }
            (b.body.clone(), b.tail.clone())
        }
        _ => return,
    };

    let b = &mut p.block_arena[bid];
    b.body.append(&mut add_insts);
    b.tail = new_tail;
}

pub fn merge_block(mut p: Program) -> Program {
    let mut arrived = Set::default();
    for bid in p.collect_used() {
        conv(bid, &mut p, &mut arrived);
    }

    p
}
