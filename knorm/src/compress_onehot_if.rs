use ty::knormal::Ty;
use util::{Id, ToSpanned};

use ast::knormal::*;

type Set = util::Set<Id>;

fn is_single(e: &Expr) -> bool {
    use ExprKind::*;
    match &e.item {
        If(..) | Let(..) | LetRec(..) | Loop { .. } => false,
        _ => true,
    }
}

fn is_small(e: &Expr) -> bool {
    use ExprKind::*;
    match &e.item {
        Let(_, e1, e2) => is_single(e1) && is_single(e2),
        If(..) | LetRec(..) | Loop { .. } => false,
        _ => true,
    }
}

fn replace_tail(
    mut e: Box<Expr>,
    onehot: bool,
    tail_onehot: Option<Box<Expr>>,
    tail_others: Box<Expr>,
) -> (Box<Expr>, Option<Box<Expr>>) {
    use ExprKind::*;
    let (item, tail_onehot) = match e.item {
        Const(ConstKind::CInt(0)) => {
            return if !onehot {
                (tail_onehot.unwrap(), None)
            } else {
                (tail_others, tail_onehot)
            }
        }
        Const(ConstKind::CInt(1)) => {
            return if onehot {
                (tail_onehot.unwrap(), None)
            } else {
                (tail_others, tail_onehot)
            }
        }
        If(kind, x, y, e1, e2) => {
            let (e1, tail_onehot) = replace_tail(e1, onehot, tail_onehot, tail_others.clone());

            let (e2, tail_onehot) = replace_tail(e2, onehot, tail_onehot, tail_others);
            (If(kind, x, y, e1, e2), tail_onehot)
        }
        Let(d, e1, e2) => {
            let (e2, tail_onehot) = replace_tail(e2, onehot, tail_onehot, tail_others);
            (Let(d, e1, e2), tail_onehot)
        }
        _ => panic!(),
    };

    e.item = item;
    (e, tail_onehot)
}

fn replace(
    e: Box<Expr>,
    cv: &Id,
    cond: Box<Expr>,
    onehot: bool,
    zeros: &mut Set,
) -> (Box<Expr>, Option<Box<Expr>>) {
    use ExprKind::*;
    let b = if let If(IfKind::IfEq, x, y, e1, e2) = &e.item {
        (x != y || x != cv)
            && ((x == cv && zeros.contains(y)) || (y == cv && zeros.contains(x)))
            && is_small(if onehot { e2 } else { e1 })
    } else {
        false
    };

    if !b {
        return (e, Some(cond));
    }

    match e.item {
        If(_, _, _, e1, e2) => {
            let (e, _) = if onehot {
                replace_tail(cond, onehot, Some(e1), e2)
            } else {
                replace_tail(cond, onehot, Some(e2), e1)
            };

            (e, None)
        }
        _ => unreachable!(),
    }
}

fn is_onehot_impl(e: &Expr, onehot: bool) -> Option<bool> {
    use ExprKind::*;

    match &e.item {
        Const(ConstKind::CInt(0)) => Some(!onehot),
        Const(ConstKind::CInt(1)) => Some(onehot),
        If(_, _, _, e1, e2) => {
            let b1 = is_onehot_impl(e1, onehot)?;
            let b2 = is_onehot_impl(e2, onehot)?;

            if b1 && b2 {
                // onehot が 2 つ以上存在
                None
            } else {
                Some(b1 || b2)
            }
        }
        Let(_, _, e2) => is_onehot_impl(e2, onehot),
        LetRec(_, _) | Loop { .. } => None, // 関数定義やループがある場合は onehot でないとみなす
        _ => None,
    }
}

// If のネストのうち一つの末尾だけが true (false) でそれ以外が全て false (true) かどうか
fn is_onehot(e: &Expr, onehot: bool) -> bool {
    is_onehot_impl(e, onehot) == Some(true)
}

// assume `name` is typed as `int` (originally `bool`)
fn occur(e: &Expr, name: &Id) -> bool {
    use ExprKind::*;
    match &e.item {
        Var(x) | UnOp(_, x) | ArrayGet(_, x) => x == name,
        BinOp(_, x, y) | CreateArray(x, y) | ArrayPut(_, x, y) => x == name || y == name,
        If(_, x, y, e1, e2) => x == name || y == name || occur(e1, name) || occur(e2, name),
        Let(_, e1, e2) | LetRec(Fundef { body: e1, .. }, e2) => occur(e1, name) || occur(e2, name),
        Tuple(xs) | App(_, xs) | ExtApp(_, xs) => xs.iter().any(|x| x == name),
        Loop { init, body, .. } => init.iter().any(|x| x == name) || occur(body, name),
        Continue(ps) => ps.iter().any(|(_, x)| x == name),
        Const(_) | ExtArray(_) | TupleGet(_, _) => false,
    }
}

fn check_follows(e: &Expr, name: &Id) -> bool {
    use ExprKind::*;
    match &e.item {
        If(_, _, _, e1, e2) => !occur(e1, name) && !occur(e2, name),
        Let(_, e1, e2) => check_follows(e1, name) && !occur(e2, name),
        _ => false,
    }
}

fn try_conv(onehot: bool, d: Decl, e1: Box<Expr>, e2: Box<Expr>, zeros: &mut Set) -> Result<Box<Expr>, (Decl, Box<Expr>, Box<Expr>)> {
    use ExprKind::*;

    if check_follows(&e2, &d.name) {
        let inner_span = e2.loc;
        match &e2.item {
            If(..) => {
                let (e2, e1) = replace(e2, &d.name, e1, onehot, zeros);
                if let Some(e1) = e1 {
                    Err((d, e1, e2))
                } else {
                    Ok(e2)
                }
            }
            Let(..) => match e2.item {
                Let(d2, ifif, e2) => {
                    let (ifif, e1) = replace(ifif, &d.name, e1, onehot, zeros);
                    let e2 = Box::new(Let(d2, ifif, conv(e2, zeros)).with_span(inner_span));
                    if let Some(e1) = e1 {
                        Err((d, e1, e2))
                    } else {
                        Ok(e2)
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    } else {
        Err((d, e1, e2))
    }
}

// 変換の呼び出し
fn conv(mut e: Box<Expr>, zeros: &mut Set) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        Let(d, e1, e2) if d.t == Ty::Int => {
            let e1 = conv(e1, zeros);
            if e1.item == Const(ConstKind::CInt(0)) {
                zeros.insert(d.name.clone());
            }

            // try true onehot
            let (d, e1, e2) = if is_onehot(&e1, true) {
                let name = d.name.clone();
                let res = try_conv(true, d, e1, e2, zeros);
                if let Ok(e) = res {
                    log::debug!("compressing onehot conditional variable `{}`", name);
                    return e;
                }
                res.unwrap_err()
            }
            else {
                (d, e1, e2)
            };
            // try false onehot
            let (d, e1, e2) = if is_onehot(&e1, false) {
                let name = d.name.clone();
                let res = try_conv(false, d, e1, e2, zeros);
                if let Ok(e) = res {
                    log::debug!("compressing onehot conditional variable `{}`", name);
                    return e;
                }
                
                res.unwrap_err()
            } else {
                (d, e1, e2)
            };

            Let(d, e1, conv(e2, zeros))
        }
        Let(d, e1, e2) => Let(d, conv(e1, zeros), conv(e2, zeros)),
        If(kind, x, y, e1, e2) => If(kind, x, y, conv(e1, zeros), conv(e2, zeros)),
        LetRec(Fundef { fvar, args, body }, e2) => LetRec(
            Fundef {
                fvar,
                args,
                body: conv(body, zeros),
            },
            conv(e2, zeros),
        ),
        Loop { vars, init, body } => Loop {
            vars,
            init,
            body: conv(body, zeros),
        },
        e => e,
    };

    e
}

// ```
// let c = if cond1 then
//   if cond2 then true
//   else false
// else false
// in
// if c = 0 then e else ()
// ```
// といった形の式を
// ```
// if cond1 then
//   if cond2 then e
//   else ()
// else ()
// ```
// に変換する。これは inline 展開の後で特に有用である
pub fn compress_onehot_if(e: Expr) -> Expr {
    *conv(Box::new(e), &mut Set::default())
}
