use util::Map as FnvMap;
use util::Id;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Saved {
    Var(Id),
    Tup(Vec<Id>)
}

type Map = FnvMap<Id, Saved>;

use ast::knormal::*;

fn conv(mut e: Box<Expr>, env: &mut Map, tyenv: &mut super::TyMap) -> Box<Expr> {
    macro_rules! map {
        ($name: expr) => {
            if let Some(Saved::Var(x)) = env.get(&$name) { x.clone() } else { $name }
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
                            log::info!("beta-reducing `{}` to `{}`", decl.name, x);
                            tyenv.remove(&decl.name);
                            env.insert(decl.name.clone(), Saved::Var(x));
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
                },
                LetKind::LetTuple(ds, x, e2) => {
                    let x = map!(x);
                    if let Some(Saved::Tup(xs)) = env.get(&x).cloned() {
                        assert!(ds.len() == xs.len());

                        log::info!("beta-reducing {:?} to {:?}", ds.iter().map(|d| &d.name).collect::<Vec<_>>(), xs);

                        for (Decl { name, t: _ }, x) in ds.into_iter().zip(xs) {
                            tyenv.remove(&name);
                            env.insert(name, Saved::Var(x));
                        }

                        return conv(e2, env, tyenv);
                    }
                    else {
                        env.insert(x.clone(), Saved::Tup(ds.iter().map(|d| d.name.clone()).collect()));
                        let e2 = conv(e2, env, tyenv);
                        LetKind::LetTuple(ds, x, e2)
                    }
                }
            };

            Let(kind)
        },
        Tuple(xs) => Tuple(xs.into_iter().map(|x| map!(x)).collect()),
        App(f, args) => App(map!(f), args.into_iter().map(|x| map!(x)).collect()),
        ExtApp(f, args) => ExtApp(f, args.into_iter().map(|x| map!(x)).collect()),
        CreateArray(num, init) => CreateArray(map!(num), map!(init)),
        Get(x, y) => Get(map!(x), map!(y)),
        Put(x, y, z) => Put(map!(x), map!(y), map!(z)),
        Loop { vars, init, cond, body } => {
            let init = init.into_iter().map(|x| map!(x)).collect();
            let cond = Condition(cond.0, map!(cond.1), map!(cond.2));
            let body = conv(body, env, tyenv);
            Loop { vars, init, cond, body }
        },
        Continue(xs) => Continue(xs.into_iter().map(|x| map!(x)).collect()),
        _ => e.item
    };

    e
}

// β 簡約を行う
// α 変換されていることを仮定している
pub fn beta_reduction(e: Expr, tyenv: &mut super::TyMap) -> Expr {
    *conv(Box::new(e), &mut Map::default(), tyenv)
}