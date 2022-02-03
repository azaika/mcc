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
        Ty::Tuple(ts) => {
            let mut acc: usize = 0;
            let mut res = Vec::new();
            for t in ts {
                res.push(acc);
                acc += type_size(t);
            }
            res.push(acc);
            res
        },
        _ => panic!("non-tuple type in tuple_offsets")
    }
}