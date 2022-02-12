use crate::mir::mir::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProgramPoint {
    pub bid: BlockId,
    pub idx: usize
}

impl ProgramPoint {
    pub fn new(bid: BlockId, idx: usize) -> Self {
        Self {
            bid,
            idx
        }
    }
}

pub type Liveness = util::Map<ProgramPoint, util::Set<usize>>;