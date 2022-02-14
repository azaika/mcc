use super::program::*;
use ast::closure::Label;
use util::{Id, Set, Map, ToSpanned};

#[derive(Debug, Clone, PartialEq)]
enum Saved {
    Nop,
    Tag(Id),
    Li(i32),
    FLi(f32),
    GetLabel(Label)
}

fn contains_call(e: &Expr) -> bool {
    use ExprKind::*;
    match &e.item {
        CallDir(..) | CallCls(..) => true,
        Let(_, e1, e2) | If(_, _, _, e1, e2) | IfF(_, _, _, e1, e2) => {
            contains_call(e1) || contains_call(e2)
        },
        Loop { body, .. } => contains_call(body),
        _ => false
    }
}

fn conv_single(e: &mut ExprKind, tyenv: &mut TyMap, safe: &Set<Id>, rename: &mut Map<Id, Id>, saved_data: &mut Map<Id, Saved>, restored: &mut Set<Id>) -> Set<(Id, Id)> {
    use ExprKind::*;

    let mut ret = Set::default();

    let mut f = |x: &mut Id| {
        if let Some(nx) = rename.get(x) {
            *x = nx.clone();
        }
        else if !safe.contains(x) {
            let nx = util::id::distinguish(x.clone());
            let t = tyenv.get(x).unwrap().clone();
            tyenv.insert(nx.clone(), t);

            let data = saved_data.get(x).unwrap().clone();
            saved_data.insert(nx.clone(), data);

            rename.insert(x.clone(), nx.clone());
            restored.insert(x.clone());
            ret.insert((x.clone(), nx.clone()));
            *x = nx;
        }
    };

    match e {
        Sw(x, Value::Var(y), z) => {
            f(x);
            f(y);
            f(z);
        },
        IntOp(_, x, Value::Var(y)) | FloatOp(_, x, y) | Lw(x, Value::Var(y)) | Sw(x, _, y) | If(_, x, Value::Var(y), _, _) | IfF(_, x, y, _, _) => {
            f(x);
            f(y);
        },
        Var(x) | UnOp(_, x) | IntOp(_, x, _) | AllocHeap(Value::Var(x)) | Out(x) | If(_, x, _, _, _) => f(x),
        Continue(ps) => {
            for (_, x) in ps {
                f(x);
            }
        },
        CallDir(_, xs) | Loop { init: xs, .. } => {
            for x in xs {
                if let Value::Var(x) = x {
                    f(x);
                }
            }
        }
        CallCls(cls, xs) => {
            f(cls);
            for x in xs {
                if let Value::Var(x) = x {
                    f(x);
                }
            }
        },
        _ => ()
    };

    ret
}

fn conv(
    mut e: Box<Expr>,
    tyenv: &mut TyMap,
    safe: &mut Set<Id>,
    rename: &mut Map<Id, Id>,
    saved_data: &mut Map<Id, Saved>,
    restored: &mut Set<Id>,
) -> Box<Expr> {
    use ExprKind::*;
    let span = e.loc;
    e.item = match e.item {
        Let(Some(v), mut e1, e2) => {
            let res = conv_single(&mut e1.item, tyenv, safe, rename, saved_data, restored);

            match &mut e1.item {
                If(_, _, _, e1, e2) => {
                    let mut safe_1 = safe.clone();

                    let mut dummy = Box::new(ExprKind::dummy());
                    std::mem::swap(e1, &mut dummy);
                    *e1 = conv(dummy, tyenv, &mut safe_1, rename, saved_data, restored);

                    let mut dummy = Box::new(ExprKind::dummy());
                    std::mem::swap(e2, &mut dummy);
                    *e2 = conv(dummy, tyenv, safe, rename, saved_data, restored);

                    safe.retain(|x| safe_1.contains(x));
                },
                IfF(_, _, _, e1, e2) => {
                    let mut safe_1 = safe.clone();

                    let mut dummy = Box::new(ExprKind::dummy());
                    std::mem::swap(e1, &mut dummy);
                    *e1 = conv(dummy, tyenv, &mut safe_1, rename, saved_data, restored);
                    
                    let mut dummy = Box::new(ExprKind::dummy());
                    std::mem::swap(e2, &mut dummy);
                    *e2 = conv(dummy, tyenv, safe, rename, saved_data, restored);

                    safe.retain(|x| safe_1.contains(x));
                },
                _ => {
                    e1 = conv(e1, tyenv, safe, rename, saved_data, restored);
                }
            };

            let data = match &e1.item {
                Nop => Saved::Nop,
                Li(i) => Saved::Li(*i),
                FLi(x) => Saved::FLi(*x),
                GetLabel(l) => Saved::GetLabel(l.clone()),
                _ => Saved::Tag(v.clone())
            };

            safe.insert(v.clone());
            saved_data.insert(v.clone(), data.clone());
            let mut e2 = conv(e2, tyenv, safe, rename, saved_data, restored);
            // insert save
            if restored.contains(&v) {
                match &data {
                    Saved::Tag(_) => {
                        e2 = Box::new(ExprKind::Let(
                            None,
                            Box::new(ExprKind::Save(v.clone(), v.clone()).with_span(span)),
                            e2
                        ).with_span(span));
                    },
                    _ => ()
                }
            }

            let item = Let(Some(v), e1, e2);
            // insert restores
            let mut e = Box::new(item.with_span(e.loc));
            for (x, nx) in res {
                let kind = match saved_data.get(&x).unwrap().clone() {
                    Saved::Nop => Nop,
                    Saved::Tag(x) => Restore(x),
                    Saved::Li(i) => Li(i),
                    Saved::FLi(x) => FLi(x),
                    Saved::GetLabel(label) => GetLabel(label),
                };
                e = Box::new(ExprKind::Let(
                    Some(nx),
                    Box::new(kind.with_span(span)),
                    e
                ).with_span(span));
            }
            return e;
        },
        Let(None, e1, e2) => {
            let e1 = conv(e1, tyenv, safe, rename, saved_data, restored);
            let e2 = conv(e2, tyenv, safe, rename, saved_data, restored);
            Let(None, e1, e2)
        },
        _ => {
            let res = conv_single(&mut e.item, tyenv, safe, rename, saved_data, restored);
            e.item = match e.item {
                If(kind, x, y, e1, e2) => {
                    let e1 = conv(e1, tyenv, safe, rename, saved_data, restored);
                    let e2 = conv(e2, tyenv, safe, rename, saved_data, restored);
                    If(kind, x, y, e1, e2)
                },
                IfF(kind, x, y, e1, e2) => {
                    let e1 = conv(e1, tyenv, safe, rename, saved_data, restored);
                    let e2 = conv(e2, tyenv, safe, rename, saved_data, restored);
                    IfF(kind, x, y, e1, e2)
                },
                CallDir(label, xs) => {
                    safe.clear();
                    CallDir(label, xs)
                },
                CallCls(f, xs) => {
                    safe.clear();
                    CallCls(f, xs)
                },
                Loop { vars, init, body } => {
                    for v in &vars {
                        saved_data.insert(v.clone(), Saved::Tag(v.clone()));
                    }
                    if contains_call(&body) {
                        safe.clear();
                    }
                    let mut body = conv(body, tyenv, safe, rename, saved_data, restored);
                    for v in &vars {
                        if restored.contains(v) {
                            body = Box::new(ExprKind::Let(
                                None,
                                Box::new(ExprKind::Save(v.clone(), v.clone()).with_span(span)),
                                body
                            ).with_span(span));
                        }
                    }
                    Loop { vars, init, body }
                },
                _ => e.item
            };

            if res.is_empty() {
                e.item
            }
            else {
                for (x, nx) in res {
                    let kind = match saved_data.get(&x).unwrap().clone() {
                        Saved::Nop => Nop,
                        Saved::Tag(x) => Restore(x),
                        Saved::Li(i) => Li(i),
                        Saved::FLi(x) => FLi(x),
                        Saved::GetLabel(label) => GetLabel(label),
                    };
                    e = Box::new(ExprKind::Let(
                        Some(nx),
                        Box::new(kind.with_span(span)),
                        e
                    ).with_span(span));
                }
                return e;
            }
        }
    };

    e
}

pub fn split_lifetime(mut p: Program) -> Program {
    for Fundef { body, args, formal_fv, .. } in &mut p.fundefs {
        // regard args as safe
        let mut safe = Set::default();
        let mut saved_data = Map::default();
        for x in args.iter().chain(formal_fv as &Vec<Id>) {
            safe.insert(x.clone());
            saved_data.insert(x.clone(), Saved::Tag(x.clone()));
        }

        let mut restored = Set::default();

        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(buf, &mut p.tyenv, &mut safe, &mut Map::default(), &mut saved_data, &mut restored);

        // insert saves
        for x in args.iter_mut().chain(formal_fv) {
            if restored.contains(x) {
                let mut buf = Box::new(ExprKind::dummy());
                std::mem::swap(body, &mut buf);
                *body = Box::new(ExprKind::Let(
                    None,
                    Box::new(ExprKind::Save(x.clone(), x.clone()).with_span(body.loc)),
                    buf
                ).with_span(body.loc));
            }
        }
    }

    p.main = conv(p.main, &mut p.tyenv, &mut Set::default(), &mut Map::default(), &mut Map::default(), &mut Set::default());

    p
}
