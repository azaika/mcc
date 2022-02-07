use crate::common;
use ast::closure::*;
use util::{Id, Set, ToSpanned};

fn is_alloc_array(e: &Expr) -> bool {
    match e.item {
        ExprKind::AllocArray(..) => true,
        _ => false,
    }
}

fn remove_assign(mut e: Box<Expr>, x: &Id) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        Let(_, e1, e2) if e1.item == Assign(Label(x.clone()), x.clone()) => {
            return e2;
        }
        _ => e.item,
    };

    e
}

fn flatten_array(
    mut e: Box<Expr>,
    independents: &Set<Id>,
    consts: &common::ConstMap,
    globals: &Set<Id>,
    tyenv: &mut TyMap,
) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        Let(v, e1, e2) if is_alloc_array(&e1) && independents.contains(&v) => {
            let (num, t, init) = match e1.item {
                AllocArray(num, t, init) => (num, t, init),
                _ => unreachable!(),
            };

            if let Some(ConstKind::CInt(s)) = consts.get(&num).cloned() {
                let s = s.max(1) as usize;
                // independent な定数長配列を変換
                tyenv.insert(v.clone(), Ty::Array(Box::new(t.clone()), s));
                if globals.contains(&v) {
                    // グローバル変数なら Assign を削除
                    let e1 = Box::new(Load(Label(v.clone())).with_span(e1.loc));
                    let e2 = remove_assign(e2, &v);
                    let e2 = flatten_array(e2, independents, consts, globals, tyenv);

                    Let(v, e1, e2)
                } else {
                    let e1 = Box::new(AllocArray(num, t, init).with_span(e1.loc));
                    let e2 = flatten_array(e2, independents, consts, globals, tyenv);

                    Let(v, e1, e2)
                }
            } else {
                let e1 = Box::new(AllocArray(num, t, init).with_span(e1.loc));
                let e2 = flatten_array(e2, independents, consts, globals, tyenv);
                Let(v, e1, e2)
            }
        }
        _ => e
            .item
            .map(|e| flatten_array(e, independents, consts, globals, tyenv)),
    };

    e
}

pub fn flatten(mut p: Program, use_strict_aliasing: bool) -> Program {
    let (independents, _) = crate::alias::analyze_aliases(&p, use_strict_aliasing);
    let consts = common::collect_consts(&p);
    let globals = p.globals.iter().map(|x| x.clone()).collect();

    p.global_init = flatten_array(
        p.global_init,
        &independents,
        &consts,
        &globals,
        &mut p.tyenv,
    );
    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = flatten_array(buf, &independents, &consts, &globals, &mut p.tyenv);
    }
    p.main = flatten_array(p.main, &independents, &consts, &globals, &mut p.tyenv);

    p
}
