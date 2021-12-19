use util::Map as FnvMap;
use util::{Id, Spanned};

type Map = FnvMap<Id, Id>;

use ast::knormal::*;

fn conv(e: Expr, env: &mut Map, tyenv: &mut super::TyMap) -> Box<Expr> {
    macro_rules! map {
        ($name: expr) => {
            env.get(&$name).map_or($name, |x| x.clone())
        }
    }

    use ExprKind::*;
    let kind = match e.item {
        Var(x) => Var(map!(x)),
        UnOp(op, x) => UnOp(op, map!(x)),
        BinOp(op, x, y) => BinOp(op, map!(x), map!(y)),
        If(kind, x, y, e1, e2) => {
            let e1 = conv(*e1, env, tyenv);
            let e2 = conv(*e2, env, tyenv);
            If(kind, map!(x), map!(y), e1, e2)
        },
        Let(l) => {
            let kind = match l {
                LetKind::Let(decl, e1, e2) => {
                    let e1 = conv(*e1, env, tyenv);
                    match e1.item {
                        Var(x) => {
                            log::info!("beta-reducing `{}` to `{}`", decl.name, x);
                            tyenv.remove(&decl.name);
                            env.insert(decl.name.clone(), x);
                            return conv(*e2, env, tyenv);
                        },
                        _ => {
                            let e2 = conv(*e2, env, tyenv);
                            LetKind::Let(decl, e1, e2)
                        }
                    }
                },
                LetKind::LetRec(fundef, e2) => {
                    let body = conv(*fundef.body, env, tyenv);
                    let e2 = conv(*e2, env, tyenv);
                    
                    let fundef = Fundef {
                        fvar: fundef.fvar,
                        args: fundef.args,
                        body
                    };

                    LetKind::LetRec(fundef, e2)
                },
                LetKind::LetTuple(ds, x, e2) => {
                    let e2 = conv(*e2, env, tyenv);

                    LetKind::LetTuple(ds, map!(x), e2)
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
        _ => e.item
    };

    Box::new(Spanned::new(kind, e.loc))
}

// β 簡約を行う
// α 変換されていることを仮定している
pub fn beta_reduction(e: Expr, tyenv: &mut super::TyMap) -> Expr {
    *conv(e, &mut Map::default(), tyenv)
}