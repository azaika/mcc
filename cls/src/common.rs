use ast::closure::*;
use util::Id;

pub type ConstMap = util::Map<Id, ConstKind>;

fn collect_consts_impl(e: &Expr, consts: &mut ConstMap) {
    use ExprKind::*;
    match &e.item {
        Let(v, e1, e2) => {
            match &e1.item {
                Const(c) => {
                    consts.insert(v.clone(), c.clone());
                }
                _ => (),
            };

            collect_consts_impl(e2, consts);
        }
        e => e.map_ref(|e| collect_consts_impl(e, consts)),
    }
}

pub fn collect_consts(p: &Program) -> ConstMap {
    let mut consts = ConstMap::default();
    collect_consts_impl(&p.global_init, &mut consts);
    for Fundef { body, .. } in &p.fundefs {
        collect_consts_impl(body, &mut consts);
    }
    collect_consts_impl(&p.main, &mut consts);
    
    consts
}
