#![feature(int_log)]

mod common;
mod virt;

pub use virt::convert;
pub use virt::simm;
pub use virt::Program;

pub fn optimize_virtual(mut p: Program) -> Program {
    let mut prev = p.clone();

    for i in 0..100 {
        log::info!("virtual opt loop: {}", i + 1);
        p = simm(p);

        if p == prev {
            break;
        }
        prev = p.clone();
    }

    p
}
