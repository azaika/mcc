mod liveness;
mod regalloc;
mod spill;
mod types;

pub use regalloc::RegAllocator;
use util::Id;

use crate::{common, mir};

fn make_varmap(tyenv: &mir::mir::TyMap) -> (util::Map<Id, usize>, util::Map<usize, Id>) {
    let mut i: usize = 0;
    let mut m1 = util::Map::default();
    let mut m2 = util::Map::default();
    for (x, _) in tyenv {
        m1.insert(x.clone(), i);
        m2.insert(i, x.clone());
        i += 1;
    }

    for r in common::REGS {
        let r = format!("%{r}");
        m1.insert(r.clone(), i);
        m2.insert(i, r.clone());
        i += 1;
    }

    (m1, m2)
}

pub fn do_regalloc() {}
