use util::{Id, ToSpanned};

type Map = util::Map<Id, Id>;

use ast::closure::*;

fn beta_label(mut e: Box<Expr>, from: &Label, to: &Label) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        Assign(l, _) if &l == from => Const(ConstKind::CUnit),
        Load(l) if &l == from => Load(to.clone()),
        e => e.map(|e| beta_label(e, from, to)),
    };

    e
}

fn conv(mut e: Box<Expr>, globals: &mut Vec<Id>, env: &mut Map, tyenv: &mut TyMap) -> Box<Expr> {
    macro_rules! map {
        ($name: expr) => {
            if let Some(x) = env.get(&$name) {
                x.clone()
            } else {
                $name
            }
        };
    }

    use ExprKind::*;
    e.item = match e.item {
        Var(x) => Var(map!(x)),
        UnOp(op, x) => UnOp(op, map!(x)),
        BinOp(op, x, y) => BinOp(op, map!(x), map!(y)),
        If(kind, x, y, e1, e2) => {
            let e1 = conv(e1, globals, env, tyenv);
            let e2 = conv(e2, globals, env, tyenv);
            If(kind, map!(x), map!(y), e1, e2)
        }
        Let(d, e1, e2) => {
            let e1 = conv(e1, globals, env, tyenv);
            match e1.item {
                Var(x) => {
                    let is_global = globals.contains(&d);
                    if !is_global || (is_global && globals.contains(&x)) {
                        log::debug!("beta-reducing `{}` to `{}`", d, x);

                        let e2 = if is_global {
                            let i = globals.iter().position(|y| y == &d).unwrap();
                            globals.remove(i);
                            beta_label(e2, &Label(d.clone()), &Label(x.clone()))
                        } else {
                            e2
                        };

                        tyenv.remove(&d);
                        env.insert(d.clone(), x);
                        return conv(e2, globals, env, tyenv);
                    } else {
                        let e2 = conv(e2, globals, env, tyenv);
                        Let(d, Box::new(Var(x).with_span(e1.loc)), e2)
                    }
                }
                _ => {
                    let e2 = conv(e2, globals, env, tyenv);
                    Let(d, e1, e2)
                }
            }
        }
        Tuple(xs) => Tuple(xs.into_iter().map(|x| map!(x)).collect()),
        CallCls(f, args) => CallCls(map!(f), args.into_iter().map(|x| map!(x)).collect()),
        CallDir(label, args) => CallDir(label, args.into_iter().map(|x| map!(x)).collect()),
        AllocArray(num, t, init) => AllocArray(map!(num), t, init.map(|x| map!(x))),
        ArrayGet(x, y) => ArrayGet(map!(x), map!(y)),
        ArrayPut(x, y, z) => ArrayPut(map!(x), map!(y), map!(z)),
        TupleGet(x, idx) => TupleGet(map!(x), idx),
        Loop { vars, init, body } => {
            let init = init.into_iter().map(|x| map!(x)).collect();
            let body = conv(body, globals, env, tyenv);
            Loop { vars, init, body }
        }
        DoAll {
            idx,
            range,
            delta,
            body,
        } => {
            let range = (map!(range.0), map!(range.1));
            let body = conv(body, globals, env, tyenv);
            DoAll {
                idx,
                range,
                delta,
                body,
            }
        }
        Continue(xs) => Continue(xs.into_iter().map(|(x, y)| (map!(x), map!(y))).collect()),
        MakeCls(label, fvs) => MakeCls(label, fvs.into_iter().map(|x| map!(x)).collect()),
        Assign(label, x) => Assign(label, x),
        Asm(inst, args) => Asm(inst, args.into_iter().map(|x| map!(x)).collect()),
        Const(_) | ExtArray(_) | Load(_) => e.item,
    };

    e
}

// β 簡約を行う
pub fn beta_reduction(mut p: Program) -> Program {
    p.global_init = conv(
        p.global_init,
        &mut p.globals,
        &mut Map::default(),
        &mut p.tyenv,
    );

    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(buf, &mut p.globals, &mut Map::default(), &mut p.tyenv);
    }

    p.main = conv(p.main, &mut p.globals, &mut Map::default(), &mut p.tyenv);

    p
}
