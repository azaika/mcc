use std::{collections::VecDeque, ops::AddAssign};

use crate::mir::mir::*;
use id_arena::Arena;
use util::{Id, Map, Set};

use super::types::*;

pub fn estimate_cost(tyenv: &TyMap, arena: &Arena<Block>, entry: BlockId) -> Map<Id, usize> {
    let mut count = Map::default();
    let mut first_depth = Map::default();
    let mut block_depth: Map<BlockId, usize> = Map::default();

    for (x, _) in tyenv {
        count.insert(x.clone(), 0);
        first_depth.insert(x.clone(), usize::MAX);
    }
    block_depth.insert(entry, 0);

    let mut visited = Set::default();
    let mut que = VecDeque::new();
    que.push_back(ProgramPoint::new(entry, 0));
    while let Some(pp) = que.pop_front() {
        if visited.contains(&pp) {
            continue;
        }

        visited.insert(pp);
        let d = block_depth.get(&pp.bid).unwrap() + pp.idx;

        let block = &arena[pp.bid];
        if pp.idx < block.body.len() {
            let (v, inst) = &block.body[pp.idx];
            if let Some(v) = v {
                if !v.starts_with("%") {
                    count.get_mut(v).unwrap().add_assign(1);
                    let v = first_depth.get_mut(v).unwrap();
                    *v = (*v).min(d);
                }
            }

            use InstKind::*;
            match &inst.item {
                Mv(x) => {
                    if !x.starts_with("%") {
                        if !count.contains_key(x) {
                            panic!("var `{x}` is not in `count`!!!!");
                        }
                        count.get_mut(x).unwrap().add_assign(1);
                    }
                }
                UnOp(_, x) | IntOp(_, x, Value::Imm(_)) | Lw(x, Value::Imm(_)) | Out(x) => {
                    count.get_mut(x).unwrap().add_assign(1);
                }
                IntOp(_, x, Value::Var(y))
                | FloatOp(_, x, y)
                | Lw(x, Value::Var(y))
                | Sw(x, Value::Imm(_), y) => {
                    count.get_mut(x).unwrap().add_assign(1);
                    count.get_mut(y).unwrap().add_assign(1);
                }
                Sw(x, Value::Var(y), z) => {
                    count.get_mut(x).unwrap().add_assign(1);
                    count.get_mut(y).unwrap().add_assign(1);
                    count.get_mut(z).unwrap().add_assign(1);
                }
                _ => (),
            }

            que.push_back(ProgramPoint::new(pp.bid, pp.idx + 1));
        } else {
            assert!(pp.idx == block.body.len());
            match &block.tail.item {
                TailKind::If(_, x, Value::Imm(_), b1, b2) => {
                    count.get_mut(x).unwrap().add_assign(1);

                    let nd1 = block_depth.entry(*b1).or_insert(usize::MAX);
                    *nd1 = (*nd1).min(d + 1);
                    let nd2 = block_depth.entry(*b2).or_insert(usize::MAX);
                    *nd2 = (*nd2).min(d + 1);

                    if !visited.contains(&ProgramPoint::new(*b1, 0)) {
                        que.push_back(ProgramPoint::new(*b1, 0));
                    }
                    if !visited.contains(&ProgramPoint::new(*b2, 0)) {
                        que.push_back(ProgramPoint::new(*b2, 0));
                    }
                }
                TailKind::If(_, x, Value::Var(y), b1, b2) | TailKind::IfF(_, x, y, b1, b2) => {
                    count.get_mut(x).unwrap().add_assign(1);
                    count.get_mut(y).unwrap().add_assign(1);

                    let nd1 = block_depth.entry(*b1).or_insert(usize::MAX);
                    *nd1 = (*nd1).min(d + 1);
                    let nd2 = block_depth.entry(*b2).or_insert(usize::MAX);
                    *nd2 = (*nd2).min(d + 1);

                    if !visited.contains(&ProgramPoint::new(*b1, 0)) {
                        que.push_back(ProgramPoint::new(*b1, 0));
                    }
                    if !visited.contains(&ProgramPoint::new(*b2, 0)) {
                        que.push_back(ProgramPoint::new(*b2, 0));
                    }
                }
                TailKind::Jump(b) => {
                    let nd = block_depth.entry(*b).or_insert(usize::MAX);
                    *nd = (*nd).min(d + 1);

                    if !visited.contains(&ProgramPoint::new(*b, 0)) {
                        que.push_back(ProgramPoint::new(*b, 0));
                    }
                }
                TailKind::Return => (),
            }
        }
    }

    let mut costs = Map::default();
    for (x, _) in tyenv {
        let f = first_depth.get(x).unwrap();
        let c = count.get(x).unwrap();

        if *f == usize::MAX {
            continue;
        }

        let cost = *f + ((*c) as usize) * 5;
        costs.insert(x.clone(), cost);
    }

    costs
}

pub fn insert_save_restore(
    _arena: &mut Arena<Block>,
    _entry: BlockId,
    _tyenv: &mut TyMap,
    _spilled: &Set<Id>,
) {
    todo!()
}
