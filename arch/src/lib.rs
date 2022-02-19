#![feature(int_log)]

mod common;
mod emit;
mod mir;
mod regalloc;
mod virt;

pub use emit::emit;
pub use mir::convert as to_mir;
pub use mir::mir::Program as Mir;
pub use regalloc::do_regalloc;
pub use virt::convert as to_virtual;
pub use virt::Program as Virtual;

pub fn optimize_virtual(mut p: Virtual) -> Virtual {
    log::info!("virtual opt started");
    p = virt::resolve_nest(p);
    p = virt::simm(p);
    p = virt::eliminate(p);
    p
}

pub fn finalize_virt(mut p: Virtual, do_opt: bool) -> Virtual {
    p = virt::split_lifetime(p);
    if do_opt {
        p = virt::eliminate(p);
    }
    p = virt::resolve_nest(p);

    p
}

pub fn optimize_mir(mut p: Mir) -> Mir {
    let mut prev = p.clone();
    for i in 0..100 {
        log::info!("mir opt loop: {}", i + 1);
        p = mir::skip_jump(p);
        p = mir::merge_block(p);
        p = mir::fold_const(p);
        p = mir::faddmul(p);
        p = mir::eliminate(p);
        p = mir::skip_jump(p);
        p = mir::merge_block(p);
        p = mir::fold_const(p);
        p = mir::faddmul(p);

        if p == prev {
            break;
        }

        prev = p.clone();
    }
    p
}
