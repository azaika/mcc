use ty::knormal::Ty;
use util::{Id, ToSpanned};

use ast::knormal::*;

fn emerge(e: &Expr, vars: &util::Set<Id>) -> bool {
    use ExprKind::*;
    match &e.item {
        Var(x) | UnOp(_, x) | TupleGet(x, _) => vars.contains(x),
        BinOp(_, x, y) | CreateArray(x, y) | ArrayGet(x, y) => vars.contains(x) || vars.contains(y),
        If(_, x, y, e1, e2) => {
            vars.contains(x) || vars.contains(y) || emerge(e1, vars) || emerge(e2, vars)
        }
        Let(_, e1, e2) | LetRec(Fundef { body: e1, .. }, e2) => {
            emerge(e1, vars) || emerge(e2, vars)
        }
        Tuple(xs) | ExtApp(_, xs) | App(_, xs) => xs.iter().any(|x| vars.contains(x)),
        ArrayPut(x, y, z) => vars.contains(x) || vars.contains(y) || vars.contains(z),
        Loop { init, body, .. } => init.iter().any(|x| vars.contains(x)) || emerge(body, vars),
        Continue(xs) => xs.iter().any(|(_, x)| vars.contains(x)),
        Const(_) | ExtArray(_) => false,
    }
}

fn has_only_exit(e: &Expr, vars: &mut util::Set<Id>) -> Option<bool> {
    use ExprKind::*;
    match &e.item {
        If(_, _, _, e1, e2) => {
            let b1 = has_only_exit(e1, vars)?;
            let b2 = has_only_exit(e2, vars)?;

            if b1 && b2 {
                None
            } else {
                Some(b1 || b2)
            }
        }
        Let(d, _, e2) => {
            vars.insert(d.name.clone());
            has_only_exit(e2, vars)
        }
        LetRec(_, _) | Loop { .. } => None, // let rec があるときや loop は諦める
        Continue(_) => Some(false),
        _ => {
            if emerge(e, &vars) {
                None
            } else {
                Some(true)
            }
        }
    }
}

fn take_tail(e: &mut Box<Expr>) -> Option<Expr> {
    use ExprKind::*;
    match &mut e.item {
        If(_, _, _, e1, e2) => take_tail(e1).or_else(|| take_tail(e2)),
        Let(_, _, e2) => take_tail(e2),
        LetRec(_, _) => panic!(),
        Continue(_) => None,
        _ => {
            let mut tail = Box::new(ExprKind::Const(ConstKind::CUnit).with_span(e.loc));
            std::mem::swap(e, &mut tail);
            Some(*tail)
        }
    }
}

fn extract_tail(mut e: Box<Expr>, tyenv: &mut TyMap) -> Box<Expr> {
    use ExprKind::*;
    let span = e.loc;
    let body = match &mut e.item {
        Loop { body, vars, .. }
            if has_only_exit(body, &mut vars.iter().map(|d| d.name.clone()).collect())
                .unwrap_or(false) =>
        {
            body
        }
        _ => return e,
    };

    let tail = take_tail(body).unwrap();

    if tail.item == ExprKind::Const(ConstKind::CUnit) {
        e
    } else {
        let d = Decl::gen_uniq(Ty::Unit);
        tyenv.insert(d.name.clone(), Ty::Unit);
        Box::new(Let(d, e, Box::new(tail)).with_span(span))
    }
}

fn conv(mut e: Box<Expr>, tyenv: &mut TyMap) -> Box<Expr> {
    use ExprKind::*;

    e.item = match e.item {
        Let(d, e1, e2) => Let(d, conv(e1, tyenv), conv(e2, tyenv)),
        LetRec(Fundef { fvar, args, body }, e2) => LetRec(
            Fundef {
                fvar,
                args,
                body: conv(body, tyenv),
            },
            conv(e2, tyenv),
        ),
        Loop { vars, init, body } => Loop {
            vars,
            init,
            body: conv(body, tyenv),
        },
        e => e,
    };

    match &e.item {
        Loop { .. } => e = extract_tail(e, tyenv),
        _ => (),
    };

    e
}

pub fn simplify_loop(e: Expr, tyenv: &mut TyMap) -> Expr {
    *conv(Box::new(e), tyenv)
}
