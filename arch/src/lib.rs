#![feature(int_log)]

mod common;
mod mir;
mod virt;

pub use mir::convert as to_mir;
pub use mir::mir::Program as Mir;
pub use virt::convert as to_virtual;
pub use virt::Program as Virtual;

pub fn optimize_virtual(mut p: Virtual) -> Virtual {
    log::info!("virtual opt started");
    p = virt::simm(p);
    p = virt::eliminate(p);
    p
}

pub fn finalize_virt(mut p: Virtual) -> Virtual {
    p = virt::resolve_nest(p);
    p
}

pub fn optimize_mir(mut p: Mir) -> Mir {
    let prev = p.clone();
    for _ in 0..100 {
        p = mir::skip_jump(p);
        p = mir::merge_block(p);
        if p == prev {
            break;
        }
    }
    p
}
