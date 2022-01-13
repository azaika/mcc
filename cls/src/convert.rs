use ast::{closure, knormal};
use util::{Id, ToSpanned};

type Set = util::Set<Id>;

// func: 自由変数としては現れてはいけないが、関数としては現れて良い関数の集合
fn has_free_impl(func: &mut Set, e: &knormal::Expr, known: &mut Set) -> bool {
    use knormal::ExprKind::*;
    match &e.item {
        Var(x) | UnOp(_, x) | TupleGet(x, _) => !known.contains(x),
        BinOp(_, x, y) | CreateArray(x, y) | ArrayGet(x, y) => {
            !known.contains(x) || !known.contains(y)
        }
        If(_, x, y, e1, e2) => {
            !known.contains(x)
                || !known.contains(y)
                || has_free_impl(func, e1, known)
                || has_free_impl(func, e2, known)
        }
        Let(d, e1, e2) => {
            if has_free_impl(func, e1, known) {
                true
            } else {
                known.insert(d.name.clone());
                let r = has_free_impl(func, e2, known);
                known.remove(&d.name);
                r
            }
        }
        LetRec(fundef, e2) => {
            for knormal::Decl { name, .. } in &fundef.args {
                known.insert(name.clone());
            }
            known.insert(fundef.fvar.name.clone());
            func.insert(fundef.fvar.name.clone());

            let r = has_free_impl(func, &fundef.body, known);

            func.remove(&fundef.fvar.name);
            known.remove(&fundef.fvar.name);
            for knormal::Decl { name, .. } in &fundef.args {
                known.remove(name);
            }

            r || has_free_impl(func, &e2, known)
        }
        Tuple(xs) | ExtApp(_, xs) => !xs.iter().all(|x| known.contains(x)),
        App(f, args) => !func.contains(f) || !args.iter().all(|x| known.contains(x)),
        ArrayPut(x, y, z) => !known.contains(x) || !known.contains(y) || !known.contains(z),
        Loop { vars, init, body } => {
            if !init.iter().all(|x| known.contains(x)) {
                return true;
            }

            for knormal::Decl { name, .. } in vars {
                known.insert(name.clone());
            }

            let r = has_free_impl(func, &body, known);

            for knormal::Decl { name, .. } in vars {
                known.remove(name);
            }

            r
        }
        Continue(xs) => !xs.iter().all(|(_, x)| known.contains(x)),
        Const(_) | ExtArray(_) => false,
    }
}

fn has_free(fundef: &knormal::Fundef, known: &Set, global: &Set) -> bool {
    let mut known: Set = known.union(global).cloned().collect();
    for knormal::Decl { name, .. } in &fundef.args {
        known.insert(name.clone());
    }

    let mut func = known.clone();
    func.insert(fundef.fvar.name.clone());
    has_free_impl(&mut func, &fundef.body, &mut known)
}

fn collect_free(e: &closure::Expr, known: &mut Set, fv: &mut Set) {
    use closure::ExprKind::*;

    let mut push = |x: &Id| {
        if !known.contains(x) {
            fv.insert(x.clone());
        }
    };
    match &e.item {
        Var(x) | UnOp(_, x) | Assign(_, x) | TupleGet(x, _) => push(x),
        BinOp(_, x, y) | CreateArray(x, y) | ArrayGet(x, y) => {
            push(x);
            push(y);
        }
        If(_, x, y, e1, e2) => {
            push(x);
            push(y);
            // if `e` is not α-formed, `known` may be broken
            collect_free(e1, known, fv);
            collect_free(e2, known, fv);
        }
        Let(d, e1, e2) => {
            collect_free(e1, known, fv);
            known.insert(d.name.clone());
            collect_free(e2, known, fv);
        }
        Tuple(xs) | CallDir(_, xs) | MakeCls(_, xs) => xs.iter().for_each(|x| push(x)),
        CallCls(x, ys) => {
            push(x);
            ys.iter().for_each(|x| push(x));
        }
        ArrayPut(x, y, z) => {
            push(x);
            push(y);
            push(z);
        }
        Loop { vars, init, body } => {
            init.iter().for_each(|x| push(x));
            for closure::Decl { name, .. } in vars {
                known.insert(name.clone());
            }
            collect_free(body, known, fv);
        }
        Continue(ps) => ps.iter().for_each(|(x, y)| {
            push(x);
            push(y)
        }),
        Const(_) | ExtArray(_) | Load(_) => { /* no vars */ }
        IntLoop { .. } => {
            unreachable!()
        }
    }
}

fn emerge(e: &knormal::Expr, name: &Id) -> bool {
    use knormal::ExprKind::*;
    match &e.item {
        Var(x) | UnOp(_, x) | TupleGet(x, _) => x == name,
        BinOp(_, x, y) | CreateArray(x, y) | ArrayGet(x, y) => x == name || y == name,
        If(_, x, y, e1, e2) => x == name || y == name || emerge(e1, name) || emerge(e2, name),
        Let(_, e1, e2) | LetRec(knormal::Fundef { body: e1, .. }, e2) => {
            emerge(e1, name) || emerge(e2, name)
        }
        Tuple(xs) | ExtApp(_, xs) | App(_, xs) => xs.iter().any(|x| x == name),
        ArrayPut(x, y, z) => x == name || y == name || z == name,
        Loop { init, body, .. } => init.iter().any(|x| x == name) || emerge(body, name),
        Continue(xs) => xs.iter().any(|(_, x)| x == name),
        Const(_) | ExtArray(_) => false,
    }
}

fn conv(
    e: Box<knormal::Expr>,
    tyenv: &knormal::TyMap,
    known: &mut Set,
    global: &Set,
    p: &mut closure::Program,
) -> Box<closure::Expr> {
    use closure::ExprKind;

    let lift = |expr: ExprKind| Box::new(expr.with_span(e.loc));
    match e.item {
        knormal::ExprKind::Const(c) => lift(ExprKind::Const(c)),
        knormal::ExprKind::Var(x) => lift(ExprKind::Var(x)),
        knormal::ExprKind::UnOp(kind, x) => lift(ExprKind::UnOp(kind, x)),
        knormal::ExprKind::BinOp(kind, x, y) => lift(ExprKind::BinOp(kind, x, y)),
        knormal::ExprKind::If(kind, x, y, e1, e2) => {
            let e1 = conv(e1, tyenv, known, global, p);
            let e2 = conv(e2, tyenv, known, global, p);

            lift(ExprKind::If(kind, x, y, e1, e2))
        }
        knormal::ExprKind::Let(d, e1, e2) => {
            let e1 = conv(e1, tyenv, known, global, p);

            if global.contains(&d.name) {
                // global variable
                p.globals.push(closure::Global {
                    name: closure::Label(d.name),
                    t: d.t,
                    init: e1,
                });

                conv(e2, tyenv, known, global, p)
            } else {
                // not global variable
                let e2 = conv(e2, tyenv, known, global, p);
                lift(ExprKind::Let(d, e1, e2))
            }
        }
        knormal::ExprKind::LetRec(fundef, e2) => {
            // 自由変数が無いなら自身は CallDir で呼ぶようにする
            let is_closure = has_free(&fundef, known, global);
            if !is_closure {
                known.insert(fundef.fvar.name.clone());
            }

            // convert function body
            let e1 = conv(fundef.body, tyenv, known, global, p);

            // free variables which are contained in converted function body
            let fvs: Vec<_> = {
                let mut known = global.clone();
                for knormal::Decl { name, .. } in &fundef.args {
                    known.insert(name.clone());
                }
                if !is_closure {
                    known.insert(fundef.fvar.name.clone());
                }

                let mut fvs = Set::default();
                collect_free(&e1, &mut known, &mut fvs);
                fvs.into_iter().collect()
            };

            let formal_fv = fvs
                .iter()
                .map(|x| closure::Decl {
                    name: x.clone(),
                    t: tyenv.get(x).unwrap().clone(),
                })
                .collect();

            p.fundefs.push(closure::Fundef {
                fvar: fundef.fvar.clone(),
                args: fundef.args,
                formal_fv,
                body: e1,
            });

            // make closure (if needed) and convert following programs
            if emerge(&e2, &fundef.fvar.name) {
                let e2 = conv(e2, tyenv, known, global, p);
                let f = fundef.fvar.name.clone();
                lift(ExprKind::Let(
                    fundef.fvar,
                    lift(ExprKind::MakeCls(
                        closure::Label(f),
                        fvs.iter().cloned().collect(),
                    )),
                    e2,
                ))
            } else {
                conv(e2, tyenv, known, global, p)
            }
        }
        knormal::ExprKind::Tuple(xs) => lift(ExprKind::Tuple(xs)),
        knormal::ExprKind::App(func, args) => {
            if known.contains(&func) {
                lift(ExprKind::CallDir(closure::Label(func), args))
            } else {
                lift(ExprKind::CallCls(func, args))
            }
        }
        knormal::ExprKind::ExtApp(func, args) => {
            lift(ExprKind::CallDir(closure::Label(func), args))
        }
        knormal::ExprKind::CreateArray(x, y) => lift(ExprKind::CreateArray(x, y)),
        knormal::ExprKind::ExtArray(x) => lift(ExprKind::ExtArray(closure::Label(x))),
        knormal::ExprKind::ArrayGet(x, y) => lift(ExprKind::ArrayGet(x, y)),
        knormal::ExprKind::ArrayPut(x, y, z) => lift(ExprKind::ArrayPut(x, y, z)),
        knormal::ExprKind::TupleGet(x, idx) => lift(ExprKind::TupleGet(x, idx)),
        knormal::ExprKind::Loop { vars, init, body } => lift(ExprKind::Loop {
            vars,
            init,
            body: conv(body, tyenv, known, global, p),
        }),
        knormal::ExprKind::Continue(ps) => lift(ExprKind::Continue(ps)),
    }
}

fn find_last_letrec(e: &knormal::Expr) -> Option<Id> {
    match &e.item {
        knormal::ExprKind::Let(_, _, e2) => find_last_letrec(e2),
        knormal::ExprKind::LetRec(fundef, e2) => {
            find_last_letrec(e2).or_else(|| Some(fundef.fvar.name.clone()))
        }
        _ => None,
    }
}

fn collect_global(e: &knormal::Expr, last: &Id, res: &mut Set) {
    match &e.item {
        knormal::ExprKind::Let(d, _, e2) => {
            log::info!("assumed `{}` as global.", d.name);
            res.insert(d.name.clone());
            collect_global(e2, last, res);
        }
        knormal::ExprKind::LetRec(fundef, e2) => {
            if &fundef.fvar.name == last {
                return;
            }
            collect_global(&e2, last, res);
        }
        _ => return,
    }
}

pub fn convert(e: knormal::Expr, tyenv: knormal::TyMap) -> closure::Program {
    let mut p = closure::Program::new();
    let last = find_last_letrec(&e);
    let mut global = Set::default();
    if let Some(last) = last {
        collect_global(&e, &last, &mut global);
    }

    p.main = conv(Box::new(e), &tyenv, &mut Set::default(), &global, &mut p);

    p
}
