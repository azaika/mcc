#![feature(int_log)]

mod common;
mod virt;

pub use virt::convert;
pub use virt::Program;

pub fn optimize_virtual(mut p: Program) -> Program {
    log::info!("virtual opt started");
    p = virt::simm(p);
    p = virt::eliminate(p);
    p
}

pub fn finalize_virt(mut p: Program) -> Program {
    p = virt::resolve_nest(p);
    p
}
