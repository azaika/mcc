use super::types::*;
use crate::{common, mir::mir::*};
use id_arena::Arena;
use util::Id as Var;
use util::{Map, Set};

type LiveId = (ProgramPoint, usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Follow {
    Zero,
    One(ProgramPoint),
    Two(ProgramPoint, ProgramPoint),
}

fn prepare_impl(
    arena: &Arena<Block>,
    entry: BlockId,
    var_idx: &Map<Var, usize>,
    def: &mut Set<LiveId>,
    used: &mut Set<LiveId>,
    follow: &mut Map<ProgramPoint, Follow>,
    prev: &mut Map<BlockId, Vec<ProgramPoint>>,
    moves: &mut Map<ProgramPoint, (usize, usize)>,
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
                Mv(x) => {
                    push(x);
                    let d = *var_idx.get(d.as_ref().unwrap()).unwrap();
                    let x = *var_idx.get(x).unwrap();
                    moves.insert(pp, (d, x));
                }
                CallDir(_) => {
                    
                    // for r in REGS {
                    //     let r = format!("%{r}");
                    //     let r = *var_idx.get(&r).unwrap();
                    //     def.insert((pp.clone(), r));
                    // }
                }
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
                UnOp(_, x)
                | IntOp(_, x, _)
                | AllocHeap(Value::Var(x))
                | Lw(x, _)
                | Out(x)
                | Save(_, x) => push(x),
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
            TailKind::Jump(b) => {
                let pp = ProgramPoint::new(bid, n);

                prev.entry(*b).or_default().push(pp.clone());

                let f = ProgramPoint::new(*b, 0);
                follow.insert(pp, Follow::One(f));
            }
            TailKind::Return => {
                let pp = ProgramPoint::new(bid, n);
                follow.insert(pp, Follow::Zero);
            }
        };
    }
}

// returns: (live_in, live_out)
fn analyze_impl(
    arena: &Arena<Block>,
    entry: BlockId,
    n: usize,
    def: &Set<LiveId>,
    used: &Set<LiveId>,
    follow: &Map<ProgramPoint, Follow>,
    prev: &Map<BlockId, Vec<ProgramPoint>>,
) -> (
    Liveness,
    Map<ProgramPoint, usize>,
    Map<ProgramPoint, Set<usize>>,
) {
    let mut live_in = Set::default();
    let mut live_out = Set::default();

    let mut queue = Set::default();
    for bid in collect_used(arena, entry) {
        let block = &arena[bid];
        for i in 0..(block.body.len() + 1) {
            let pp = ProgramPoint::new(bid, i);
            for j in 0..n {
                {
                    let k = (pp, j);
                    let d = def.contains(&k);
                    let u = used.contains(&k);

                    let b = (live_out.contains(&k) && !d) || u;
                    if b != live_in.contains(&k) {
                        if b {
                            live_in.insert(k);
                        } else {
                            live_in.remove(&k);
                        }
                        if i == 0 {
                            for next in prev.get(&pp.bid).iter().flat_map(|x| *x) {
                                queue.insert(((next.clone(), j), false));
                            }
                        } else {
                            queue.insert(((ProgramPoint::new(bid, i - 1), j), false));
                        }
                    }
                }
                {
                    let k = (pp, j);
                    let b = match follow.get(&pp).unwrap() {
                        Follow::Zero => false,
                        Follow::One(p1) => live_in.contains(&(p1.clone(), j)),
                        Follow::Two(p1, p2) => {
                            live_in.contains(&(p1.clone(), j)) || live_in.contains(&(p2.clone(), j))
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

            let b = (live_out.contains(&k) && !d) || u;
            if b != live_in.contains(&k) {
                if b {
                    live_in.insert(k);
                } else {
                    live_in.remove(&k);
                }
                if pp.idx == 0 {
                    for next in prev.get(&pp.bid).iter().flat_map(|x| *x) {
                        queue.insert(((next.clone(), i_v), false));
                    }
                } else {
                    queue.insert(((ProgramPoint::new(pp.bid, pp.idx - 1), i_v), false));
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

    let mut out: Liveness = Map::default();
    for (pp, i_v) in live_out {
        out.entry(pp).or_default().insert(i_v);
    }

    let mut defs = Map::default();
    for (pp, i_v) in def {
        defs.insert(pp.clone(), *i_v);
    }
    let mut uses: Map<ProgramPoint, Set<_>> = Map::default();
    for (pp, i_v) in used {
        uses.entry(pp.clone()).or_default().insert(*i_v);
    }

    (out, defs, uses)
}

fn build_graph(
    arena: &Arena<Block>,
    entry: BlockId,
    n: usize,
    live_out: &Liveness,
    moves: &Map<ProgramPoint, (usize, usize)>,
    def: &Map<ProgramPoint, usize>,
    used: &Map<ProgramPoint, Set<usize>>,
    precolored: &Map<usize, common::Color>,
) -> (Vec<Set<usize>>, Set<(usize, usize)>, Vec<usize>) {
    let mut edges: Vec<Set<usize>> = vec![Set::default(); n];
    let mut all_edges: Set<(usize, usize)> = Set::default();
    let mut degrees: Vec<usize> = vec![0; n];

    for bid in collect_used(arena, entry) {
        let tail_idx = arena[bid].body.len();
        let mut live: Set<_> = live_out
            .get(&ProgramPoint::new(bid, tail_idx))
            .into_iter()
            .flatten()
            .cloned()
            .collect();

        for idx in (0..(tail_idx + 1)).rev() {
            let pp = ProgramPoint::new(bid, idx);

            if let Some((x, y)) = moves.get(&pp) {
                if x == y {
                    continue;
                }

                for u in used.get(&pp).into_iter().flatten() {
                    live.remove(u);
                }
            }

            if let Some(d) = def.get(&pp) {
                for v in &live {
                    let d = *d;
                    let v = *v;
                    if d == v || all_edges.contains(&(d, v)) {
                        continue;
                    }
                    all_edges.insert((d, v));
                    all_edges.insert((v, d));

                    if !precolored.contains_key(&d) {
                        edges[d].insert(v);
                        degrees[d] += 1;
                    }
                    if !precolored.contains_key(&v) {
                        edges[v].insert(d);
                        degrees[v] += 1;
                    }
                }

                live.remove(d);
            }

            for u in used.get(&pp).into_iter().flatten() {
                live.insert(*u);
            }
        }
    }

    (edges, all_edges, degrees)
}

pub fn analyze(
    arena: &Arena<Block>,
    entry: BlockId,
    n: usize,
    var_idx: &Map<Var, usize>,
    idx_var: &Map<usize, Var>,
    precolored: &Map<usize, common::Color>,
) -> (
    Map<ProgramPoint, (usize, usize)>,
    Vec<Set<usize>>,
    Set<(usize, usize)>,
    Vec<usize>,
) {
    let mut def = Set::default();
    let mut used = Set::default();
    let mut follow = Map::default();
    let mut prev = Map::default();
    let mut moves = Map::default();
    prepare_impl(
        arena,
        entry,
        var_idx,
        &mut def,
        &mut used,
        &mut follow,
        &mut prev,
        &mut moves,
    );

    let (live_out, def, used) = analyze_impl(arena, entry, n, &def, &used, &follow, &prev);

    for (pp , live) in &live_out {
        println!("{}[{}] = {:#?}", arena[pp.bid].name, pp.idx, live.iter().map(|x| idx_var.get(x).unwrap()).collect::<Vec<&Var>>());
    }

    let (edges, all_edges, degrees) =
        build_graph(arena, entry, n, &live_out, &moves, &def, &used, precolored);

    (moves, edges, all_edges, degrees)
}
