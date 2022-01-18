use ast::closure::*;
use ty::knormal::Ty;
use util::{Id, ToSpanned};

type Map<T> = util::Map<Id, T>;

fn has_continue(e: &Expr) -> bool {
    match &e.item {
        ExprKind::If(_, _, _, e1, e2) | ExprKind::Let(_, e1, e2) => {
            has_continue(e1) || has_continue(e2)
        }
        ExprKind::Continue(_) => true,
        ExprKind::Loop { .. } | ExprKind::DoAll { .. } => false,
        _ => false,
    }
}

fn is_delta(
    kind: &BinOpKind,
    v: &Id,
    x: &Id,
    y: &Id,
    deltas: &Map<i32>,
    constant: &Map<i32>,
) -> Option<i32> {
    match kind {
        BinOpKind::Add => {
            if x == v || y == v {
                let x = if x == v { y } else { x };
                constant.get(x).cloned()
            } else if let Some(delta) = deltas.get(x) {
                constant.get(y).map(|x| *x + delta)
            } else if let Some(delta) = deltas.get(y) {
                constant.get(x).map(|x| *x + delta)
            } else {
                None
            }
        }
        BinOpKind::Sub if x == v => {
            if x == v {
                constant.get(y).map(|y| -y)
            } else if let Some(delta) = deltas.get(x) {
                constant.get(y).map(|y| delta - y)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn find_delta(e: &Expr, v: &Id, deltas: &mut Map<i32>, constants: &mut Map<i32>) -> Option<i32> {
    use ExprKind::*;
    match &e.item {
        If(_, _, _, e1, e2) => {
            let delta = find_delta(e1, v, deltas, constants)?;
            find_delta(e2, v, deltas, constants).filter(|d| *d == delta)
        }
        Let(d, e1, e2) => {
            match &e1.item {
                Const(ConstKind::CInt(i)) => {
                    constants.insert(d.name.clone(), *i);
                }
                BinOp(kind, x, y) if x == v || y == v => {
                    if let Some(delta) = is_delta(kind, v, x, y, deltas, constants) {
                        deltas.insert(d.name.clone(), delta);
                    }
                }
                Var(x) => {
                    if let Some(x) = constants.get(x) {
                        let x = *x;
                        constants.insert(d.name.clone(), x);
                    } else if let Some(x) = deltas.get(x) {
                        let x = *x;
                        deltas.insert(d.name.clone(), x);
                    }
                }
                _ => { /* do nothing */ }
            };

            find_delta(e2, v, deltas, constants)
        }
        Continue(xs) => {
            assert!(xs.len() == 1);
            let (_, x) = &xs[0];
            deltas
                .get(x)
                .cloned()
                .or_else(|| if x == v { Some(0) } else { None })
        }
        _ => None,
    }
}

fn is_e1_exit(
    v: &Id,
    constants: &mut Map<i32>,
    kind: &IfKind,
    e1: &Expr,
    e2: &Expr,
) -> Option<i32> {
    if kind == &IfKind::IfEq {
        return None;
    }

    let b1 = !has_continue(e1);
    let b2 = if b1 { false } else { !has_continue(e2) };

    if !b1 && !b2 {
        return None;
    }

    find_delta(
        if b1 { &e2 } else { &e1 },
        &v,
        &mut Map::default(),
        constants,
    )
}

fn conv(mut e: Box<Expr>, constants: &mut Map<i32>) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        If(kind, x, y, e1, e2) => If(kind, x, y, conv(e1, constants), conv(e2, constants)),
        Let(d, e1, e2) => {
            let e1 = conv(e1, constants);
            if let Const(ConstKind::CInt(i)) = &e1.item {
                constants.insert(d.name.clone(), *i);
            }

            Let(d, e1, conv(e2, constants))
        }
        Loop { vars, init, body } => {
            if vars.len() != 1 || vars[0].t != Ty::Int {
                Loop {
                    vars,
                    init,
                    body: conv(body, constants),
                }
            } else {
                let v = vars[0].name.clone();
                let init = init.into_iter().next().unwrap();
                match body.item {
                    If(kind, x, y, e1, e2) if x != y && (x == v || y == v) => {
                        if let Some(delta) = is_e1_exit(&v, constants, &kind, &e1, &e2) {
                            let end = if x == v { y.clone() } else { x.clone() };
                            ExprKind::DoAll {
                                idx: Decl::new(v, Ty::Int),
                                range: (init, end),
                                delta,
                                body: Box::new(
                                    ExprKind::If(IfKind::IfLE, x, y, e1, e2).with_span(body.loc),
                                ),
                            }
                        } else {
                            Loop {
                                vars,
                                init: vec![init],
                                body: conv(
                                    Box::new(If(kind, x, y, e1, e2).with_span(body.loc)),
                                    constants,
                                ),
                            }
                        }
                    }
                    _ => Loop {
                        vars,
                        init: vec![init],
                        body: conv(body, constants),
                    },
                }
            }
        }
        DoAll {
            idx,
            range,
            delta,
            body,
        } => DoAll {
            idx,
            range,
            delta,
            body: conv(body, constants),
        },
        _ => e.item,
    };

    e
}

pub fn detect_intloop(mut p: Program) -> Program {
    let mut constants = Map::default();
    for g in &mut p.globals {
        if let ExprKind::Const(ConstKind::CInt(x)) = &g.init.item {
            constants.insert(g.name.0.clone(), *x);
        } else {
            let mut inner = Box::new(ExprKind::Const(ConstKind::CUnit).with_span((0, 0)));
            std::mem::swap(&mut g.init, &mut inner);
            g.init = conv(inner, &mut constants);
        }
    }

    p.main = conv(p.main, &mut constants);
    for fundef in &mut p.fundefs {
        let mut inner = Box::new(ExprKind::Const(ConstKind::CUnit).with_span((0, 0)));
        std::mem::swap(&mut fundef.body, &mut inner);
        fundef.body = conv(inner, &mut constants);
    }

    p
}
