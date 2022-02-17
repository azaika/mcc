use crate::common;
use ast::closure::*;
use util::{id, Id, Set, ToSpanned};

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

fn gen_array_init(
    arr: Id,
    size: usize,
    init: Id,
    span: util::Span,
    tyenv: &mut TyMap,
    e2: Box<Expr>,
) -> Box<Expr> {
    let mut gen_new = |t: Ty| -> Id {
        let v = id::gen_tmp_var_with(t.short());
        tyenv.insert(v.clone(), t);
        v
    };
    let lift = |e: ExprKind| -> Box<Expr> { Box::new(e.with_span(span)) };

    use ExprKind::*;
    if size < 10 {
        let mut e2 = e2;
        for idx in (0..size).rev() {
            let tmp = gen_new(Ty::Unit);
            let idx_v = gen_new(Ty::Int);

            e2 = lift(Let(
                tmp,
                lift(ArrayPut(arr.clone(), idx_v.clone(), init.clone())),
                e2,
            ));
            e2 = lift(Let(idx_v, lift(Const(ConstKind::CInt(idx as i32))), e2));
        }

        e2
    } else {
        let idx = gen_new(Ty::Int);
        let idx_next = gen_new(Ty::Int);
        let zero = gen_new(Ty::Int);
        let one = gen_new(Ty::Int);
        let end = gen_new(Ty::Int);
        let tmp1 = gen_new(Ty::Unit);
        let tmp2 = gen_new(Ty::Unit);

        let cont = lift(Let(
            idx_next.clone(),
            lift(BinOp(BinOpKind::Add, idx.clone(), one.clone())),
            lift(Continue(vec![(idx.clone(), idx_next)])),
        ));

        let body = lift(Let(
            tmp1,
            lift(ArrayPut(arr.clone(), idx.clone(), init)),
            cont,
        ));
        let body = lift(If(
            IfKind::IfLE,
            idx.clone(),
            end.clone(),
            body,
            lift(Const(ConstKind::CUnit)),
        ));

        let init_loop = lift(DoAll {
            idx,
            range: (zero.clone(), end.clone()),
            delta: 1,
            body,
        });

        let e2 = lift(Let(tmp2, init_loop, e2));
        let e2 = lift(Let(end, lift(Const(ConstKind::CInt(size as i32 - 1))), e2));
        let e2 = lift(Let(one, lift(Const(ConstKind::CInt(1))), e2));
        lift(Let(zero, lift(Const(ConstKind::CInt(0))), e2))
    }
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
                    let mut e2 = remove_assign(e2, &v);
                    e2 = flatten_array(e2, independents, consts, globals, tyenv);
                    // add initizalize loop
                    if let Some(init) = init {
                        e2 = gen_array_init(v.clone(), s, init, e1.loc, tyenv, e2);
                    }

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
        e => e.map(|e| flatten_array(e, independents, consts, globals, tyenv)),
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
