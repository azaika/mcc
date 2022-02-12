use id_arena::Arena;

use super::mir::*;

type Set = util::Set<BlockId>;

fn can_skip(b: &Block) -> Option<BlockId> {
    let jump = match b.tail.item {
        TailKind::Jump(jump) => jump,
        _ => return None,
    };

    if b.body.len() >= 2 {
        None
    } else if b.body.is_empty() {
        Some(jump)
    } else {
        let (_, inst) = &b.body[0];
        if inst.item == InstKind::Nop {
            Some(jump)
        } else {
            None
        }
    }
}

fn conv(bid: BlockId, arena: &mut Arena<Block>, arrived: &mut Set) {
    if arrived.contains(&bid) {
        return;
    }
    arrived.insert(bid);

    let (o1, o2) = match arena[bid].tail.item {
        TailKind::If(_, _, _, b1, b2) | TailKind::IfF(_, _, _, b1, b2) => {
            conv(b1, arena, arrived);
            conv(b2, arena, arrived);
            (can_skip(&arena[b1]), can_skip(&arena[b2]))
        }
        TailKind::Jump(b) => {
            conv(b, arena, arrived);
            (can_skip(&arena[b]), None)
        }
        TailKind::Return => return,
    };

    let name = arena[bid].name.clone();
    let item = &mut arena[bid].tail.item;
    match item {
        TailKind::If(_, _, _, b1, b2) | TailKind::IfF(_, _, _, b1, b2) => {
            *b1 = o1.unwrap_or(*b1);
            *b2 = o2.unwrap_or(*b2);
        }
        TailKind::Jump(b) => {
            if o1.is_some() {
                log::debug!("skipping `{name}`.");
            }
            *b = o1.unwrap_or(*b);
        }
        TailKind::Return => unreachable!(),
    }
}

pub fn skip_jump(mut p: Program) -> Program {
    let mut arrived = Set::default();
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
