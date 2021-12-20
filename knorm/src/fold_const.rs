use util::{Spanned, Id};

use ast::knormal::*;
use crate::TyMap;
use ty::knormal::Ty;

#[derive(Debug, Clone)]
enum Const {
    Val(ConstKind),
    Tup(Vec<Id>)
}

type ConstMap = util::Map<Id, Const>;

fn get_int(consts: &ConstMap, x: &Id) -> Option<i32> {
    match consts.get(x) {
        Some(Const::Val(ConstKind::CInt(i))) => Some(*i),
        _ => None
    }
}

fn get_float(consts: &ConstMap, x: &Id) -> Option<f32> {
    match consts.get(x) {
        Some(Const::Val(ConstKind::CFloat(d))) => Some(*d),
        _ => None
    }
}

fn get_tup<'a>(consts: &'a ConstMap, x: &Id) -> Option<&'a Vec<Id>> {
    match consts.get(x) {
        Some(Const::Tup(ys)) => Some(ys),
        _ => None
    }
}

// consts: 定数テーブル
fn conv(mut e: Box<Expr>, tyenv: &mut TyMap, consts: &mut ConstMap) -> Box<Expr> {
    use ConstKind::*;
    e.item = match e.item {
        ExprKind::UnOp(op, x) => {
            use UnOpKind::*;
            let r = match op {
                Neg => get_int(consts, &x).map(|i| CInt(-i)),
                FNeg => get_float(consts, &x).map(|d| CFloat(d)),
            };

            match r {
                Some(c) => ExprKind::Const(c),
                None => ExprKind::UnOp(op, x)
            }
        },
        ExprKind::BinOp(op, x, y) => {
            use BinOpKind::*;

            let r_i = get_int(consts, &x).map(|i1|get_int(consts, &y).map(|i2|
                CInt(match op {
                    Add => i1 + i2,
                    Sub => i1 - i2,
                    Mul => i1 * i2,
                    Div => i1 / i2,
                    _ => unreachable!()
                })
            )).flatten();
            let r_f = get_float(consts, &x).map(|d1| get_float(consts, &y).map(|d2| {
                CFloat(match op {
                    FAdd => d1 + d2,
                    FSub => d1 - d2,
                    FMul => d1 * d2,
                    FDiv => d1 / d2,
                    _ => unreachable!()
                })
            })).flatten();

            match r_i.or(r_f) {
                Some(c) => ExprKind::Const(c),
                None => ExprKind::BinOp(op, x, y)
            }
        },
        ExprKind::If(kind, x, y, e1, e2) => {
            let e1 = conv(e1, tyenv, consts);
            let e2 = conv(e2, tyenv, consts);
            ExprKind::If(kind, x, y, e1, e2)
        },
        ExprKind::Let(l) => ExprKind::Let(
            match l {
                LetKind::Let(mut d, e1, e2) => {
                    let e1 = conv(e1, tyenv, consts);
                    // e1 が定数式だったら定数テーブルを更新
                    match &e1.item {
                        ExprKind::Const(CUnit) => (),
                        ExprKind::Const(c) => {
                            consts.insert(d.name.clone(), Const::Val(c.clone()));
                        },
                        ExprKind::Var(x) => {
                            // 変数の定数性を伝播
                            if let Some(c) = consts.get(x).map(|c| c.clone()) {
                                consts.insert(d.name.clone(), c);
                            }
                        },
                        ExprKind::Tuple(xs) => {
                            consts.insert(d.name.clone(), Const::Tup(xs.clone()));
                        },
                        ExprKind::CreateArray(num, _) => {
                            // if-let-chain を使うともっと簡潔になるが, まだ unstable
                            // https://github.com/rust-lang/rust/issues/53667
                            if let Ty::Array(t, None) = d.t {
                                d.t = if let Some(Const::Val(CInt(s))) = consts.get(num) {
                                    let s = *s;
                                    if s >= 0 {
                                        let new_t = Ty::Array(t, Some(s as usize));
                                        // 型情報を更新
                                        tyenv.get_mut(&d.name).map(|t| *t = new_t.clone());
                                        new_t
                                    }
                                    else {
                                        log::warn!("negative sized array created in {}:{}", e.loc.0, e.loc.1);
                                        Ty::Array(t, None)
                                    }
                                }
                                else {
                                    Ty::Array(t, None)
                                };
                            }
                        },
                        _ => ()
                    };
                    
                    let e2 = conv(e2, tyenv, consts);
                    LetKind::Let(d, e1, e2)
                },
                LetKind::LetTuple(ds, x, e2) => {
                    if let Some(xs) = get_tup(consts, &x) {
                        // `let (d1, ..., dn) = x in e2` で `x = (x1, ..., xn)` と分かっているとき
                        // ```
                        // let x1 = y1 in
                        // ...
                        // let xn = yn in
                        // e2
                        // ```
                        // に式を置き換える
                        assert!(ds.len() == xs.len());
                        let v: Vec<_> = ds.into_iter().zip(xs.clone()).collect();
                        let new_e = v.into_iter().rev().fold(
                            e2,
                            |cont, (d, x)| {
                                Box::new(Spanned::new(
                                    ExprKind::Let(LetKind::Let(
                                        d,
                                        Box::new(Spanned::new(ExprKind::Var(x), e.loc)),
                                        cont
                                    )),
                                    e.loc
                                ))
                            }
                        );

                        return conv(new_e, tyenv, consts);
                    }
                    else {
                        let e2 = conv(e2, tyenv, consts);
                        LetKind::LetTuple(ds, x, e2)
                    }
                },
                _ => l.map(|e| conv(e, tyenv, consts))
            }
        ),
        _ => e.item
    };

    e
}

// 定数畳み込みを行う
// CreateArray が定数で呼ばれていることが分かる場合, 型変換を行う
pub fn fold_const(e: Expr, tyenv: &mut TyMap) -> Expr {
    *conv(Box::new(e), tyenv, &mut ConstMap::default())
}