use ast::mir::*;

type Set = util::Set<BlockId>;

fn can_skip(b: &Block) -> Option<BlockId> {
    let jump = match b.tail.item {
        TailKind::Jump(jump) => jump,
        _ => return None
    };

    if b.body.len() >= 2 {
        None
    }
    else if b.body.is_empty() {
        Some(jump)
    }
    else {
        let (dest, inst) = &b.body[0];
        if dest.is_none() && inst.item == InstKind::Const(ConstKind::CUnit) {
            Some(jump)
        }
        else {
            None
        }
    }
}

fn conv(bid: BlockId, p: &mut Program, arrived: &mut Set) {
    if arrived.contains(&bid) {
        return;
    }
    arrived.insert(bid);

    let (o1, o2) = match p.block_arena[bid].tail.item {
        TailKind::If(_, _, _, b1, b2) | TailKind::ForEach(_, _, _, b1, b2) => {
            conv(b1, p, arrived);
            conv(b2, p, arrived);
            (can_skip(&p.block_arena[b1]), can_skip(&p.block_arena[b2]))
        },
        TailKind::Jump(b) => {
            conv(b, p, arrived);
            (can_skip(&p.block_arena[b]), None)
        },
        TailKind::Return(_) => return,
    };

    let name = p.block_arena[bid].name.clone();
    let item = &mut p.block_arena[bid].tail.item;
    match item {
        TailKind::If(_, _ , _, b1, b2) => {
            *b1 = o1.unwrap_or(*b1);
            *b2 = o2.unwrap_or(*b2);
        },
        TailKind::ForEach(_, _, _, _, b2) => {
            let b = o2.unwrap_or(*b2);
            if o1.is_some() {
                // unnecessary loop
                log::debug!("eliminating for-each `{name}`.");
                *item = TailKind::Jump(b)
            }
            else {
                *b2 = b;
            }
        },
        TailKind::Jump(b) => {
            if o1.is_some() {
                log::debug!("skipping `{name}`.");
            }
            *b = o1.unwrap_or(*b);
        },
        TailKind::Return(_) => unreachable!(),
    }
}

pub fn compress_jump(mut p: Program) -> Program {
    let mut arrived = Set::default();
    for bid in p.collect_used() {
        conv(bid, &mut p, &mut arrived);
    }

    p
}