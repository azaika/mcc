use super::types::*;
use crate::mir::mir::*;
use id_arena::Arena;
use util::Id as Var;

type LiveId = (ProgramPoint, usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Follow {
    Zero,
    One(ProgramPoint),
    Two(ProgramPoint, ProgramPoint),
}

fn gen_varmap(p: &Program) -> (util::Map<Var, usize>, util::Map<usize, Var>) {
    let mut i: usize = 0;
    let mut m1 = util::Map::default();
    let mut m2 = util::Map::default();
    for (x, _) in &p.tymap {
        m1.insert(x.clone(), i);
        m2.insert(i, x.clone());
        i += 1;
    }

    (m1, m2)
}

pub fn prepare(
    arena: &Arena<Block>,
    entry: BlockId,
    var_idx: &util::Map<Var, usize>,
    def: &mut util::Set<LiveId>,
    used: &mut util::Set<LiveId>,
    follow: &mut util::Map<ProgramPoint, Follow>,
    prev: &mut util::Map<BlockId, Vec<ProgramPoint>>,
) {
    for bid in collect_used(arena, entry) {
        let block = &arena[bid];
        let n = block.body.len();
        for (idx, (d, inst)) in block.body.iter().enumerate() {
            let pp = ProgramPoint::new(bid, idx);
            follow.insert(pp.clone(), Follow::One(ProgramPoint::new(bid, idx + 1)));

            if let Some(d) = d {
                let d = *var_idx.get(d).unwrap();
                def.insert((pp.clone(), d));
            }

            let mut push = |x: &Var| {
                let x = *var_idx.get(x).unwrap();
                used.insert((pp.clone(), x));
            };

            use InstKind::*;
            match &inst.item {
                Sw(x, Value::Var(y), z) => {
                    push(x);
                    push(y);
                    push(z);
                }
                IntOp(_, x, Value::Var(y))
                | FloatOp(_, x, y)
                | Lw(x, Value::Var(y))
                | Sw(x, _, y) => {
                    push(x);
                    push(y);
                }
                Mv(x)
                | UnOp(_, x)
                | IntOp(_, x, _)
                | AllocHeap(Value::Var(x))
                | Lw(x, _)
                | Out(x) => push(x),
                CallDir(_, xs) => {
                    for x in xs {
                        push(x);
                    }
                }
                CallCls(f, xs) => {
                    push(f);
                    for x in xs {
                        push(x);
                    }
                }
                _ => (),
            }
        }

        match &block.tail.item {
            TailKind::If(_, x, y, b1, b2) => {
                let pp = ProgramPoint::new(bid, n);

                let x = *var_idx.get(x).unwrap();
                used.insert((pp.clone(), x));

                if let Value::Var(y) = y {
                    let y = *var_idx.get(y).unwrap();
                    used.insert((pp.clone(), y));
                }

                prev.entry(*b1).or_default().push(pp.clone());
                prev.entry(*b2).or_default().push(pp.clone());

                let f1 = ProgramPoint::new(*b1, 0);
                let f2 = ProgramPoint::new(*b2, 0);
                follow.insert(pp.clone(), Follow::Two(f1, f2));
            }
            TailKind::IfF(_, x, y, b1, b2) => {
                let pp = ProgramPoint::new(bid, n);

                let x = *var_idx.get(x).unwrap();
                used.insert((pp.clone(), x));

                let y = *var_idx.get(y).unwrap();
                used.insert((pp.clone(), y));

                prev.entry(*b1).or_default().push(pp.clone());
                prev.entry(*b2).or_default().push(pp.clone());

                let f1 = ProgramPoint::new(*b1, 0);
                let f2 = ProgramPoint::new(*b2, 0);
                follow.insert(pp, Follow::Two(f1, f2));
            }
            TailKind::Return(x) => {
                if let Some(x) = x {
                    let x = *var_idx.get(x).unwrap();
                    used.insert((ProgramPoint::new(bid, n), x));
                }
            }
            TailKind::Jump(b) => {
                let pp = ProgramPoint::new(bid, n);

                prev.entry(*b).or_default().push(pp.clone());

                let f = ProgramPoint::new(*b, 0);
                follow.insert(pp, Follow::One(f));
            }
        };
    }
}

// returns: (live_in, live_out)
pub fn analyze(
    arena: &Arena<Block>,
    entry: BlockId,
    n: usize,
    def: &util::Set<LiveId>,
    used: &util::Set<LiveId>,
    follow: &util::Map<ProgramPoint, Follow>,
    prev: &util::Map<BlockId, Vec<ProgramPoint>>,
) -> (Liveness, Liveness) {
    let mut live_in = util::Set::default();
    let mut live_out = util::Set::default();

    let mut queue = util::Set::with_capacity_and_hasher(arena.len() * 4, util::Hasher::default());
    for bid in collect_used(arena, entry) {
        let block = &arena[bid];
        for i in 0..(block.body.len()) {
            let pp = ProgramPoint::new(bid, i);
            for j in 0..n {
                queue.insert(((pp, j), false));
                queue.insert(((pp, j), true));
            }
        }
    }

    while !queue.is_empty() {
        let ((pp, i_v), is_in) = {
            let a = queue.iter().next().unwrap().clone();
            queue.take(&a).unwrap()
        };

        if is_in {
            let k = (pp, i_v);
            let d = def.contains(&k);
            let u = used.contains(&k);

            let b = live_out.contains(&k) && !d || u;
            if b != live_in.contains(&k) {
                if b {
                    live_in.insert(k);
                } else {
                    live_in.remove(&k);
                }
                if i_v == 0 {
                    for next in prev.get(&pp.bid).unwrap() {
                        queue.insert(((next.clone(), i_v), false));
                    }
                }
            }
        } else {
            let k = (pp, i_v);
            let b = match follow.get(&pp).unwrap() {
                Follow::Zero => false,
                Follow::One(p1) => live_in.contains(&(p1.clone(), i_v)),
                Follow::Two(p1, p2) => {
                    live_in.contains(&(p1.clone(), i_v)) || live_in.contains(&(p2.clone(), i_v))
                }
            };
            if b != live_out.contains(&k) {
                if b {
                    live_out.insert(k.clone());
                } else {
                    live_out.remove(&k);
                }
                queue.insert((k, true));
            }
        }
    }

    let mut r_in: Liveness = util::Map::default();
    let mut r_out: Liveness = util::Map::default();
    for (pp, i_v) in live_in {
        r_in.entry(pp).or_default().insert(i_v);
    }
    for (pp, i_v) in live_out {
        r_out.entry(pp).or_default().insert(i_v);
    }

    for bid in collect_used(arena, entry) {
        let block = &arena[bid];
        for i in 0..(block.body.len()) {
            let pp = ProgramPoint::new(bid, i);
            r_in.entry(pp).or_default();
            r_in.entry(pp).or_default();
        }
    }

    (r_in, r_out)
}
