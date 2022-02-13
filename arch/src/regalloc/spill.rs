use crate::mir::mir::*;
use id_arena::Arena;
use util::{Id, Set};

pub fn insert_save_restore(
    arena: &mut Arena<Block>,
    entry: BlockId,
    tyenv: &mut TyMap,
    spilled: &Set<Id>,
) {
    todo!()
}
