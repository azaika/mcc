use util::Map as FnvMap;
use util::{Id, Spanned, id};

type Map = FnvMap<Id, Id>;

use ast::knormal::*;

pub fn rename(e: Box<Expr>, env: &mut Map) -> Box<Expr> {
    macro_rules! map {
        ($name: expr) => {
            if let Some(_x) = env.get(&$name) { _x.clone() } else { $name.clone() }
        }
    }

    use ExprKind::*;
    let kind = match e.item {
        Var(x) => Var(map!(x)),
        UnOp(op, x) => UnOp(op, map!(x)),
        BinOp(op, x, y) => BinOp(op, map!(x), map!(y)),
        If(kind, x, y, e1, e2) => {
            let e1 = rename(e1, env);
            let e2 = rename(e2, env);
            If(kind, map!(x), map!(y), e1, e2)
        },
        Let(l) => {
            let kind = match l {
                LetKind::Let(decl, e1, e2) => {
                    let e1 = rename(e1, env);

                    let new_name = id::distinguish(decl.name.clone());
                    let s = env.insert(decl.name.clone(), new_name.clone());
                    let e2 = rename(e2, env);
                    util::restore(env, &decl.name, s);
                    
                    LetKind::Let(Decl::new(new_name, decl.t), e1, e2)
                },
                LetKind::LetRec(fundef, e2) => {
                    let decl = fundef.fvar;
                    let new_name = id::distinguish(decl.name.clone());
                    let new_args: Vec<_> = fundef.args.iter().map(|d| id::distinguish(d.name.clone())).collect();

                    let old_f = env.insert(decl.name.clone(), new_name.clone());

                    let e2 =  rename(e2, env);

                    let mut old_args = vec![];
                    for (Decl{ name, t: _ }, x) in fundef.args.iter().zip(&new_args) {
                        old_args.push(env.insert(name.clone(), x.clone()));
                    }

                    let body =  rename(fundef.body, env);

                    for (x, d) in old_args.into_iter().zip(&fundef.args) {
                        util::restore(env, &d.name, x);
                    }
                    util::restore(env, &decl.name, old_f);

                    let args = new_args.into_iter().zip(fundef.args).map(|(x, d)| Decl::new(x, d.t)).collect();

                    let fundef = Fundef {
                        fvar: Decl::new(new_name, decl.t),
                        args,
                        body
                    };

                    LetKind::LetRec(fundef, e2)
                },
                LetKind::LetTuple(ds, x, e2) => {
                    let new_names: Vec<_> = ds.iter().map(|d| id::distinguish(d.name.clone())).collect();

                    let mut old_vars = vec![];
                    for (Decl{ name, t: _ }, x) in ds.iter().zip(&new_names) {
                        old_vars.push(env.insert(name.clone(), x.clone()));
                    }

                    let e2 = rename(e2, env);

                    for (x, d) in old_vars.into_iter().zip(&ds) {
                        util::restore(env, &d.name, x);
                    }

                    let ds = new_names.into_iter().zip(ds).map(|(x, d)| Decl::new(x, d.t)).collect();

                    LetKind::LetTuple(ds, x, e2)
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
