use ast::closure::*;
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
                    constants.insert(d.clone(), *i);
                }
                BinOp(kind, x, y) if x == v || y == v => {
                    if let Some(delta) = is_delta(kind, v, x, y, deltas, constants) {
                        deltas.insert(d.clone(), delta);
                    }
                }
                Var(x) => {
                    if let Some(x) = constants.get(x) {
                        let x = *x;
                        constants.insert(d.clone(), x);
                    } else if let Some(x) = deltas.get(x) {
                        let x = *x;
                        deltas.insert(d.clone(), x);
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

// returns (delta, should_shift_end)
fn is_e1_exit(
    v: &Id,
    x: &Id,
    y: &Id,
    constants: &mut Map<i32>,
    kind: &IfKind,
    e1: &Expr,
    e2: &Expr,
) -> Option<(i32, bool)> {
    if kind == &IfKind::IfEq {
        return None;
    }

    let b1 = !has_continue(e1);
    let b2 = if b1 { false } else { !has_continue(e2) };

    if !b1 && !b2 {
        return None;
    }

    let delta = find_delta(
        if b1 { &e2 } else { &e1 },
        &v,
        &mut Map::default(),
        constants,
    )?;

    if delta == 0 {
        panic!("infinite loop detected on unreachable code");
    }
    if delta > 0 {
        if v == y && b1 {
            Some((delta, true))
        } else if v == x && b2 {
            Some((delta, false))
        } else {
            None
        }
    } else {
        if v == x && b1 {
            Some((delta, true))
        } else if v == y && b2 {
            Some((delta, false))
        } else {
            None
        }
    }
}

fn conv(mut e: Box<Expr>, constants: &mut Map<i32>, tyenv: &mut TyMap) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        If(kind, x, y, e1, e2) => If(
            kind,
            x,
            y,
            conv(e1, constants, tyenv),
            conv(e2, constants, tyenv),
        ),
        Let(d, e1, e2) => {
            let e1 = conv(e1, constants, tyenv);
            if let Const(ConstKind::CInt(i)) = &e1.item {
                constants.insert(d.clone(), *i);
            }

            Let(d, e1, conv(e2, constants, tyenv))
        }
        Loop { vars, init, body } => {
            if vars.len() != 1 || tyenv.get(&vars[0]).unwrap() != &Ty::Int {
                Loop {
                    vars,
                    init,
                    body: conv(body, constants, tyenv),
                }
            } else {
                let v = vars[0].clone();
                let init = init.into_iter().next().unwrap();
                match body.item {
                    If(kind, x, y, e1, e2) if x != y && (x == v || y == v) => {
                        if let Some((delta, do_shift)) =
                            is_e1_exit(&v, &x, &y, constants, &kind, &e1, &e2)
                        {
                            let end = if do_shift {
                                let end = util::id::gen_tmp_var_with(Ty::Int.short());
                                tyenv.insert(end.clone(), Ty::Int);
                                end
                            } else {
                                if x == v {
                                    y.clone()
                                } else {
                                    x.clone()
                                }
                            };
                            tyenv.insert(v.clone(), Ty::Int);

                            log::debug!("detected Do-All loop, which uses `{v}`");
                            let mut e = Box::new(
                                DoAll {
                                    idx: v,
                                    range: (init, end.clone()),
                                    delta,
                                    body: conv(
                                        Box::new(
                                            If(IfKind::IfLE, x, y, e1, e2).with_span(body.loc),
                                        ),
                                        constants,
                                        tyenv,
                                    ),
                                }
                                .with_span(e.loc),
                            );

                            if do_shift {
                                let loc = e.loc;
                                let kind = if delta > 0 {
                                    BinOpKind::Sub
                                } else {
                                    BinOpKind::Add
                                };
                                let one = util::id::gen_tmp_var_with(Ty::Int.short());
                                tyenv.insert(end.clone(), Ty::Int);
                                e = Box::new(
                                    Let(
                                        one.clone(),
                                        Box::new(Const(1.into()).with_span(e.loc)),
                                        Box::new(
                                            Let(
                                                end.clone(),
                                                Box::new(
                                                    BinOp(kind, end.clone(), one.clone())
                                                        .with_span(loc),
                                                ),
                                                e,
                                            )
                                            .with_span(loc),
                                        ),
                                    )
                                    .with_span(loc),
                                );
                            }

                            return e;
                        } else {
                            Loop {
                                vars,
                                init: vec![init],
                                body: conv(
                                    Box::new(If(kind, x, y, e1, e2).with_span(body.loc)),
                                    constants,
                                    tyenv,
                                ),
                            }
                        }
                    }
                    _ => Loop {
                        vars,
                        init: vec![init],
                        body: conv(body, constants, tyenv),
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
            body: conv(body, constants, tyenv),
        },
        _ => e.item,
    };

    e
}

pub fn detect_doall(mut p: Program) -> Program {
    let mut constants = Map::default();
    {
        let mut buf = Box::new(ExprKind::Const(ConstKind::CUnit).with_span((0, 0)));
        std::mem::swap(&mut p.global_init, &mut buf);
        p.global_init = conv(buf, &mut constants, &mut p.tyenv);
    }

    for fundef in &mut p.fundefs {
        let mut inner = Box::new(ExprKind::Const(ConstKind::CUnit).with_span((0, 0)));
        std::mem::swap(&mut fundef.body, &mut inner);
        fundef.body = conv(inner, &mut constants, &mut p.tyenv);
    }

    p.main = conv(p.main, &mut constants, &mut p.tyenv);

    p
}
