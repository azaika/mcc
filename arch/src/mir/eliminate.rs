use crate::regalloc::{analyze_liveout, make_idmap, ProgramPoint};

use super::mir::*;
use id_arena::Arena;
use util::{Id, Map, Set};

fn has_effect(e: &Inst) -> bool {
    use InstKind::*;
    match &e.item {
        Nop | Li(_) | FLi(_) | FloatOp(..) | IntOp(..) | UnOp(..) => false,
        _ => true
    }
}

fn conv(bid: BlockId, arena: &mut Arena<Block>, live_out: &Map<ProgramPoint, Set<Id>>) {
    let mut buf = vec![];
    std::mem::swap(&mut buf, &mut arena[bid].body);
    arena[bid].body = buf
        .into_iter()
        .enumerate()
        .filter_map(|(idx, (v, inst))| {
            if v.is_none() {
                return Some((v, inst));
            }

            if let Some(live) = live_out.get(&ProgramPoint::new(bid, idx)) {
                let v = v.as_ref().unwrap();
                if !live.contains(v) && !has_effect(&inst) {
                    return None;
                }
            }

            Some((v, inst))
        })
        .collect();
}

pub fn eliminate(mut p: Program) -> Program {
    let (var_idx, idx_var) = make_idmap(&p.tymap);
    let n = var_idx.len();
    {
        log::debug!("elimination starts to main_arena");
        let live_out = analyze_liveout(&p.main_arena, p.entry, n, &var_idx, &idx_var);
        for bid in p.collect_main_used() {
            conv(bid, &mut p.main_arena, &live_out);
        }
    }

    for f in &mut p.fundefs {
        log::debug!("elimination starts to {}", f.name);
        let live_out = analyze_liveout(&f.block_arena, f.entry, n, &var_idx, &idx_var);
        for bid in f.collect_used() {
            conv(bid, &mut f.block_arena, &live_out);
        }
    }

    p
}
