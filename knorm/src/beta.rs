use util::Map as FnvMap;
use util::Id;

type Map = FnvMap<Id, Id>;

use ast::knormal::*;

fn conv(mut e: Box<Expr>, env: &mut Map, tyenv: &mut super::TyMap) -> Box<Expr> {
    macro_rules! map {
        ($name: expr) => {
            if let Some(x) = env.get(&$name) { x.clone() } else { $name }
        }
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
        },
        Let(l) => {
            let kind = match l {
                LetKind::Let(decl, e1, e2) => {
                    let e1 = conv(e1, env, tyenv);
                    match e1.item {
                        Var(x) => {
                            log::debug!("beta-reducing `{}` to `{}`", decl.name, x);
                            tyenv.remove(&decl.name);
                            env.insert(decl.name.clone(), x);
                            return conv(e2, env, tyenv);
                        },
                        _ => {
                            let e2 = conv(e2, env, tyenv);
                            LetKind::Let(decl, e1, e2)
                        }
                    }
                },
                LetKind::LetRec(fundef, e2) => {
                    let body = conv(fundef.body, env, tyenv);
                    let e2 = conv(e2, env, tyenv);
                    
                    let fundef = Fundef {
                        fvar: fundef.fvar,
                        args: fundef.args,
                        body
                    };

                    LetKind::LetRec(fundef, e2)
                }
            };

            Let(kind)
        },
        Tuple(xs) => Tuple(xs.into_iter().map(|x| map!(x)).collect()),
        App(f, args) => App(map!(f), args.into_iter().map(|x| map!(x)).collect()),
        ExtApp(f, args) => ExtApp(f, args.into_iter().map(|x| map!(x)).collect()),
        CreateArray(num, init) => CreateArray(map!(num), map!(init)),
        ArrayGet(x, y) => ArrayGet(map!(x), map!(y)),
        ArrayPut(x, y, z) => ArrayPut(map!(x), map!(y), map!(z)),
        TupleGet(x, idx) => TupleGet(map!(x), idx),
        Loop { vars, init, body } => {
            let init = init.into_iter().map(|x| map!(x)).collect();
            let body = conv(body, env, tyenv);
            Loop { vars, init, body }
        },
        Continue(xs) => Continue(xs.into_iter().map(|(x, y)| (map!(x), map!(y))).collect()),
        _ => e.item
    };

    e
}

// β 簡約を行う
// α 変換されていることを仮定している
pub fn beta_reduction(e: Expr, tyenv: &mut super::TyMap) -> Expr {
    *conv(Box::new(e), &mut Map::default(), tyenv)
}