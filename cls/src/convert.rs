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
        Tuple(xs) | ExtApp(_, xs) | Asm(_, xs) | AsmE(_, xs) => {
            !xs.iter().all(|x| known.contains(x))
        }
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
        Var(x) | UnOp(_, x) | Assign(_, x) | TupleGet(x, _) | AllocArray(x, _) => push(x),
        BinOp(_, x, y) | ArrayGet(x, y) => {
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
            known.insert(d.clone());
            collect_free(e2, known, fv);
        }
        Tuple(xs) | CallDir(_, xs) | MakeCls(_, xs) | Asm(_, xs) => xs.iter().for_each(|x| push(x)),
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
            for name in vars {
                known.insert(name.clone());
            }
            collect_free(body, known, fv);
        }
        Continue(ps) => ps.iter().for_each(|(x, y)| {
            push(x);
            push(y)
        }),
        DoAll {
            idx, range, body, ..
        } => {
            known.insert(idx.clone());
            known.insert(range.0.clone());
            known.insert(range.1.clone());
            collect_free(body, known, fv);
        }
        Const(_) | ExtArray(_) | Load(_) => { /* no vars */ }
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
        Tuple(xs) | ExtApp(_, xs) | App(_, xs) | Asm(_, xs) | AsmE(_, xs) => {
            xs.iter().any(|x| x == name)
        }
        ArrayPut(x, y, z) => x == name || y == name || z == name,
        Loop { init, body, .. } => init.iter().any(|x| x == name) || emerge(body, name),
        Continue(xs) => xs.iter().any(|(_, x)| x == name),
        Const(_) | ExtArray(_) => false,
    }
}

fn is_create_array(e: &knormal::Expr) -> bool {
    match e.item {
        knormal::ExprKind::CreateArray(..) => true,
        _ => false,
    }
}

fn gen_new_var(p: &mut closure::Program, t: closure::Ty) -> Id {
    let x = util::id::gen_tmp_var_with(t.short());
    p.tyenv.insert(x.clone(), t);
    x
}

fn gen_array_init(
    p: &mut closure::Program,
    arr: Id,
    span: util::Span,
    num: Id,
    init: Id,
    cont: Box<closure::Expr>,
) -> Box<closure::Expr> {
    use closure::ExprKind;
    use ExprKind::*;

    let lift = |expr: ExprKind| Box::new(expr.with_span(span));

    let zero = gen_new_var(p, closure::Ty::Int);
    let one = gen_new_var(p, closure::Ty::Int);
    let end = gen_new_var(p, closure::Ty::Int);
    let idx = gen_new_var(p, closure::Ty::Int);
    let tmp = gen_new_var(p, closure::Ty::Unit);
    let tmp2 = gen_new_var(p, closure::Ty::Unit);
    let next = gen_new_var(p, closure::Ty::Int);

    let cont = lift(Let(
        tmp,
        lift(DoAll {
            idx: idx.clone(),
            range: (zero.clone(), end.clone()),
            delta: 1,
            body: lift(Let(
                tmp2,
                lift(ArrayPut(arr, idx.clone(), init)),
                lift(Let(
                    next.clone(),
                    lift(BinOp(closure::BinOpKind::Add, idx.clone(), one.clone())),
                    lift(Continue(vec![(idx.clone(), next.clone())]))
                ))
            )),
        }),
        cont,
    ));
    let cont = lift(Let(
        end,
        lift(BinOp(closure::BinOpKind::Sub, num, one.clone())),
        cont,
    ));

    lift(Let(
        zero,
        lift(Const(0.into())),
        lift(Let(one, lift(Const(1.into())), cont)),
    ))
}

fn insert_global<F: FnOnce(&mut closure::Program, Box<closure::Expr>) -> Box<closure::Expr>>(
    p: &mut closure::Program,
    name: Id,
    k: F,
) {
    p.globals.push(name.clone());

    let mut buf = Box::new(closure::ExprKind::dummy());
    std::mem::swap(&mut buf, &mut p.global_init);
    p.global_init = k(p, buf);
}

fn conv(
    e: Box<knormal::Expr>,
    tyenv: &knormal::TyMap,
    known: &mut Set,
    zeros: &mut Set,
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
            let e1 = conv(e1, tyenv, known, zeros, global, p);
            let e2 = conv(e2, tyenv, known, zeros, global, p);

            lift(ExprKind::If(kind, x, y, e1, e2))
        }
        // create_array を alloc_array に変換
        knormal::ExprKind::Let(d, e1, e2) if is_create_array(&e1) => {
            let (num, init) = match e1.item {
                knormal::ExprKind::CreateArray(num, init) => (num, init),
                _ => unreachable!(),
            };

            let t = match &d.t {
                ty::knormal::Ty::Array(t) => (**t).clone().into(),
                _ => panic!(),
            };
            let e1 = Box::new(ExprKind::AllocArray(num.clone(), t).with_span(e1.loc));
            p.tyenv.insert(d.name.clone(), d.t.clone().into());

            let is_dummy = zeros.contains(&num);

            if global.contains(&d.name) {
                // global variable
                let name = d.name.clone();
                let e2 = conv(e2, tyenv, known, zeros, global, p);
                insert_global(p, d.name, |p, cont| {
                    let e2 = if is_dummy {
                        // dummy array は初期化しない
                        cont
                    } else {
                        gen_array_init(p, name.clone(), e.loc, num, init, cont)
                    };
                    let tmp = gen_new_var(p, closure::Ty::Unit);
                    let e2 = lift(ExprKind::Let(
                        tmp,
                        lift(ExprKind::Assign(closure::Label(name.clone()), name.clone())),
                        e2,
                    ));

                    lift(ExprKind::Let(name, e1, e2))
                });

                e2
            } else {
                // non-global variable
                let e2 = conv(e2, tyenv, known, zeros, global, p);
                let e2 = if is_dummy {
                    // dummy array は初期化しない
                    e2
                } else {
                    gen_array_init(p, d.name.clone(), e.loc, num, init, e2)
                };
                lift(ExprKind::Let(d.name, e1, e2))
            }
        }
        knormal::ExprKind::Let(d, e1, e2) => {
            let e1 = conv(e1, tyenv, known, zeros, global, p);

            // 0 と分かるものだけ記憶 (dummy array の初期化省略に使う)
            if e1.item == ExprKind::Const(0.into()) {
                zeros.insert(d.name.clone());
            }

            p.tyenv.insert(d.name.clone(), d.t.clone().into());

            if global.contains(&d.name) {
                // global variable
                let name = d.name.clone();
                let e2 = conv(e2, tyenv, known, zeros, global, p);
                insert_global(p, d.name.clone(), |p, cont| {
                    let tmp = gen_new_var(p, closure::Ty::Unit);
                    let e2 = lift(ExprKind::Let(
                        tmp,
                        lift(ExprKind::Assign(closure::Label(name.clone()), name.clone())),
                        cont,
                    ));
                    lift(ExprKind::Let(name, e1, e2))
                });

                e2
            } else {
                // non-global variable
                let e2 = conv(e2, tyenv, known, zeros, global, p);
                lift(ExprKind::Let(d.name, e1, e2))
            }
        }
        knormal::ExprKind::LetRec(fundef, e2) => {
            // 自由変数が無いなら自身は CallDir で呼ぶようにする
            let is_closure = has_free(&fundef, known, global);
            if !is_closure {
                known.insert(fundef.fvar.name.clone());
            }

            // convert function body
            let e1 = conv(fundef.body, tyenv, known, zeros, global, p);

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

            for x in &fvs {
                p.tyenv
                    .insert(x.clone(), tyenv.get(x).unwrap().clone().into());
            }

            for d in &fundef.args {
                p.tyenv.insert(d.name.clone(), d.t.clone().into());
            }

            let formal_fv = fvs.clone();

            p.fundefs.push(closure::Fundef {
                name: fundef.fvar.name.clone(),
                args: fundef.args.into_iter().map(|d| d.name).collect(),
                formal_fv,
                body: e1,
            });

            // make closure (if needed) and convert following programs
            if emerge(&e2, &fundef.fvar.name) {
                let e2 = conv(e2, tyenv, known, zeros, global, p);
                let f = fundef.fvar.name.clone();
                lift(ExprKind::Let(
                    fundef.fvar.name,
                    lift(ExprKind::MakeCls(closure::Label(f), fvs)),
                    e2,
                ))
            } else {
                conv(e2, tyenv, known, zeros, global, p)
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
        knormal::ExprKind::CreateArray(num, init) => {
            let t: closure::Ty = tyenv.get(&init).unwrap().clone().into();

            let arr = gen_new_var(p, closure::Ty::ArrayPtr(Box::new(t.clone())));

            let e1 = lift(ExprKind::Var(arr.clone()));
            if zeros.contains(&num) {
                e1
            } else {
                let e1 = gen_array_init(p, arr.clone(), e.loc, num.clone(), init, e1);
                lift(ExprKind::Let(arr, lift(ExprKind::AllocArray(num, t)), e1))
            }
        }
        knormal::ExprKind::ExtArray(x) => lift(ExprKind::ExtArray(closure::Label(x))),
        knormal::ExprKind::ArrayGet(x, y) => lift(ExprKind::ArrayGet(x, y)),
        knormal::ExprKind::ArrayPut(x, y, z) => lift(ExprKind::ArrayPut(x, y, z)),
        knormal::ExprKind::TupleGet(x, idx) => lift(ExprKind::TupleGet(x, idx)),
        knormal::ExprKind::Loop { vars, init, body } => {
            for d in &vars {
                p.tyenv.insert(d.name.clone(), d.t.clone().into());
            }

            lift(ExprKind::Loop {
                vars: vars.into_iter().map(|d| d.name).collect(),
                init,
                body: conv(body, tyenv, known, zeros, global, p),
            })
        }
        knormal::ExprKind::Continue(ps) => lift(ExprKind::Continue(ps)),
        knormal::ExprKind::Asm(inst, args) | knormal::ExprKind::AsmE(inst, args) => {
            lift(ExprKind::Asm(inst, args))
        }
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

    p.main = conv(
        Box::new(e),
        &tyenv,
        &mut Set::default(),
        &mut Set::default(),
        &global,
        &mut p,
    );

    p.globals.reverse();

    p
}
