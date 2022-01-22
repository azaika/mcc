use util::Id;

type Map = util::Map<Id, Id>;

use ast::closure::*;

fn conv(mut e: Box<Expr>, env: &mut Map, tyenv: &mut TyMap) -> Box<Expr> {
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
            let e1 = conv(e1, env, tyenv);
            let e2 = conv(e2, env, tyenv);
            If(kind, map!(x), map!(y), e1, e2)
        }
        Let(d, e1, e2) => {
            let e1 = conv(e1, env, tyenv);
            match e1.item {
                Var(x) => {
                    log::debug!("beta-reducing `{}` to `{}`", d, x);
                    tyenv.remove(&d);
                    env.insert(d.clone(), x);
                    return conv(e2, env, tyenv);
                }
                _ => {
                    let e2 = conv(e2, env, tyenv);
                    Let(d, e1, e2)
                }
            }
        }
        Tuple(xs) => Tuple(xs.into_iter().map(|x| map!(x)).collect()),
        CallCls(f, args) => CallCls(map!(f), args.into_iter().map(|x| map!(x)).collect()),
        CallDir(label, args) => CallDir(label, args.into_iter().map(|x| map!(x)).collect()),
        AllocArray(num, t) => AllocArray(map!(num), t),
        ArrayGet(x, y) => ArrayGet(map!(x), map!(y)),
        ArrayPut(x, y, z) => ArrayPut(map!(x), map!(y), map!(z)),
        TupleGet(x, idx) => TupleGet(map!(x), idx),
        Loop { vars, init, body } => {
            let init = init.into_iter().map(|x| map!(x)).collect();
            let body = conv(body, env, tyenv);
            Loop { vars, init, body }
        }
        DoAll {
            idx,
            range,
            delta,
            body,
        } => {
            let range = (map!(range.0), map!(range.1));
            let body = conv(body, env, tyenv);
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
        _ => e.item,
    };

    e
}

// β 簡約を行う
// α 変換されていることを仮定している
pub fn beta_reduction(mut p: Program) -> Program {
    for g in &mut p.globals {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(&mut g.init, &mut buf);
        g.init = conv(buf, &mut Map::default(), &mut p.tyenv);
    }

    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(buf, &mut Map::default(), &mut p.tyenv);
    }

    p.main = conv(p.main, &mut Map::default(), &mut p.tyenv);

    p
}
