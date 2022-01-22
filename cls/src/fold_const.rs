use util::Id;

use ast::closure::*;

type ConstMap = util::Map<Id, ConstKind>;

fn get_int(consts: &ConstMap, x: &Id) -> Option<i32> {
    match consts.get(x) {
        Some(ConstKind::CInt(i)) => Some(*i),
        _ => None,
    }
}

fn get_float(consts: &ConstMap, x: &Id) -> Option<f32> {
    match consts.get(x) {
        Some(ConstKind::CFloat(d)) => Some(*d),
        _ => None,
    }
}

fn is_alloc_array(e: &Expr) -> bool {
    match e.item {
        ExprKind::AllocArray(_, _) => true,
        _ => false,
    }
}

// consts: 定数テーブル
fn conv(mut e: Box<Expr>, tyenv: &mut TyMap, consts: &mut ConstMap) -> Box<Expr> {
    use ast::knormal::ConstKind::*;
    e.item = match e.item {
        ExprKind::Var(x) => {
            if let Some(i) = get_int(consts, &x) {
                ExprKind::Const(i.into())
            } else if let Some(d) = get_float(consts, &x) {
                ExprKind::Const(d.into())
            } else {
                ExprKind::Var(x)
            }
        }
        ExprKind::UnOp(op, x) => {
            use ast::knormal::UnOpKind::*;
            let r = match op {
                Neg => get_int(consts, &x).map(|i| CInt(-i)),
                FNeg => get_float(consts, &x).map(|d| CFloat(d)),
            };

            match r {
                Some(c) => ExprKind::Const(c),
                None => ExprKind::UnOp(op, x),
            }
        }
        ExprKind::BinOp(op, x, y) => {
            use ast::knormal::BinOpKind::*;

            let r_i = get_int(consts, &x)
                .map(|i1| {
                    get_int(consts, &y).map(|i2| {
                        CInt(match op {
                            Add => i1 + i2,
                            Sub => i1 - i2,
                            Mul => i1 * i2,
                            Div => i1 / i2,
                            _ => unreachable!(),
                        })
                    })
                })
                .flatten();
            let r_f = get_float(consts, &x)
                .map(|d1| {
                    get_float(consts, &y).map(|d2| {
                        CFloat(match op {
                            FAdd => d1 + d2,
                            FSub => d1 - d2,
                            FMul => d1 * d2,
                            FDiv => d1 / d2,
                            _ => unreachable!(),
                        })
                    })
                })
                .flatten();

            match r_i.or(r_f) {
                Some(c) => ExprKind::Const(c),
                None => ExprKind::BinOp(op, x, y),
            }
        }
        ExprKind::If(kind, x, y, e1, e2) => {
            let xy_int = get_int(consts, &x).zip(get_int(consts, &y));
            let xy_float = get_float(consts, &x).zip(get_float(consts, &y));

            let cond = match kind {
                IfKind::IfEq => xy_int
                    .map(|(x, y)| x == y)
                    .or(xy_float.map(|(x, y)| x == y)),
                IfKind::IfLE => xy_float
                    .map(|(x, y)| x <= y)
                    .or(xy_float.map(|(x, y)| x <= y)),
            };

            if let Some(b) = cond {
                if b {
                    return conv(e1, tyenv, consts);
                } else {
                    return conv(e2, tyenv, consts);
                }
            }

            let e1 = conv(e1, tyenv, consts);
            let e2 = conv(e2, tyenv, consts);

            ExprKind::If(kind, x, y, e1, e2)
        }
        ExprKind::Let(d, e1, e2) if is_alloc_array(&e1) => {
            let (num, t) = match &e1.item {
                ExprKind::AllocArray(num, t) => (num, t),
                _ => unreachable!(),
            };

            if let Some(s) = get_int(consts, num) {
                let arr_t = tyenv.get_mut(&d).unwrap();
                // if s < 0, the result is undefined
                // so we regard `s` as 1
                if s < 0 {
                    log::warn!("detected negative sized array `{d}`");
                }
                *arr_t = Ty::Array(Box::new(t.clone()), s.max(1) as usize);
            };

            ExprKind::Let(d, e1, conv(e2, tyenv, consts))
        }
        ExprKind::Let(d, e1, e2) => {
            let e1 = conv(e1, tyenv, consts);
            // e1 が定数式だったら定数テーブルを更新
            match &e1.item {
                ExprKind::Const(CUnit) => (),
                ExprKind::Const(c) => {
                    consts.insert(d.clone(), c.clone());
                }
                ExprKind::Var(x) => {
                    // 変数の定数性を伝播
                    if let Some(c) = consts.get(x).map(|c| c.clone()) {
                        consts.insert(d.clone(), c);
                    }
                }
                _ => (),
            };

            let e2 = conv(e2, tyenv, consts);
            ExprKind::Let(d, e1, e2)
        }
        ExprKind::Loop { vars, init, body } => ExprKind::Loop {
            vars,
            init,
            body: conv(body, tyenv, consts),
        },
        _ => e.item,
    };

    e
}

// 定数畳み込みを行う
pub fn fold_const(mut p: Program) -> Program {
    let mut consts = ConstMap::default();
    {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(&mut p.global_init, &mut buf);
        p.global_init = conv(buf, &mut p.tyenv, &mut consts);
    }

    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(buf, &mut p.tyenv, &mut consts);
    }

    p.main = conv(p.main, &mut p.tyenv, &mut consts);

    p
}
