use ast::closure::Ty;

pub fn type_size(t: &Ty) -> usize {
    // depending heavily on ISA
    match t {
        Ty::Tuple(ts) => ts.iter().map(type_size).sum(),
        Ty::Array(t, s) => s * type_size(t),
        _ => 1,
    }
}

pub fn tuple_offsets(t: &Ty) -> Vec<usize> {
    match t {
        Ty::Tuple(ts) | Ty::TuplePtr(ts) => {
            let mut acc: usize = 0;
            let mut res = Vec::new();
            for t in ts {
                res.push(acc);
                acc += type_size(t);
            }
            res.push(acc);
            res
        }
        _ => panic!("non-tuple type in tuple_offsets"),
    }
}

#[allow(dead_code)]
pub static REG_ZERO: &'static str = "r0";
#[allow(dead_code)]
pub static REG_STACK: &'static str = "r3";
#[allow(dead_code)]
pub static REG_HEAP: &'static str = "r4";
pub static REGS: &'static [&'static str] = &[
    "r1", "r2", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", "r16",
    "r17", "r18", "r19", "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29",
    "r30", "r31", "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39", "r40", "r41", "r42",
    "r43", "r44", "r45", "r46", "r47", "r48", "r49", "r50", "r51", "r52", "r53", "r54", "r55",
    "r56", "r57", "r58", "r59", "r60", "r61", "r62",
];
#[allow(dead_code)]
pub static REG_TMP: &'static str = "r63";
