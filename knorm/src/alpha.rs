use util::Map as FnvMap;
use util::{Id, id};

use ty::knormal::Ty;

type Map = FnvMap<Id, Id>;
pub type TyMap = FnvMap<Id, Ty>;

use ast::knormal::*;

pub fn conv(mut e: Box<Expr>, env: &mut Map) -> Box<Expr> {
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
            let e1 = conv(e1, env);
            let e2 = conv(e2, env);
            If(kind, map!(x), map!(y), e1, e2)
        },
        Let(l) => {
            let kind = match l {
                LetKind::Let(decl, e1, e2) => {
                    let e1 = conv(e1, env);

                    let new_name = id::distinguish(decl.name.clone());
                    let s = env.insert(decl.name.clone(), new_name.clone());
                    let e2 = conv(e2, env);
                    util::restore(env, &decl.name, s);
                    
                    LetKind::Let(Decl::new(new_name, decl.t), e1, e2)
                },
                LetKind::LetRec(fundef, e2) => {
                    let decl = fundef.fvar;
                    let new_name = id::distinguish(decl.name.clone());
                    let new_args: Vec<_> = fundef.args.iter().map(|d| id::distinguish(d.name.clone())).collect();

                    let old_f = env.insert(decl.name.clone(), new_name.clone());

                    let e2 =  conv(e2, env);

                    let mut old_args = vec![];
                    for (Decl{ name, t: _ }, x) in fundef.args.iter().zip(&new_args) {
                        old_args.push(env.insert(name.clone(), x.clone()));
                    }

                    let body =  conv(fundef.body, env);

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
        Loop { vars, loop_vars, init, body }  => {
            let new_names: Vec<_> = vars.iter().map(|d| id::distinguish(d.name.clone())).collect();
            let new_lvnames: Vec<_> = loop_vars.iter().map(|d| id::distinguish(d.name.clone())).collect();

            let mut old_vars = vec![];
            for (Decl{ name, .. }, x) in vars.iter().zip(&new_names) {
                old_vars.push(env.insert(name.clone(), x.clone()));
            }
            let mut old_lvs = vec![];
            for (Decl{ name, .. }, x) in loop_vars.iter().zip(&new_lvnames) {
                old_lvs.push(env.insert(name.clone(), x.clone()));
            }

            let body = conv(body, env);

            for (x, d) in old_lvs.into_iter().zip(&loop_vars) {
                util::restore(env, &d.name, x);
            }
            for (x, d) in old_vars.into_iter().zip(&vars) {
                util::restore(env, &d.name, x);
            }

            let vars = new_names.into_iter().zip(vars).map(|(x, d)| Decl::new(x, d.t)).collect();
            let loop_vars = new_lvnames.into_iter().zip(loop_vars).map(|(x, d)| Decl::new(x, d.t)).collect();
            let init = init.into_iter().map(|x| map!(x)).collect();
            
            Loop { vars, loop_vars, init, body }
        },
        Continue(xs) => Continue(xs.into_iter().map(|(x, y)| (map!(x), map!(y))).collect()),
        Const(_) | ExtArray(_) => e.item,
    };

    e
}

// α 変換を行う
// α 変換後の式と, 変数名から型への対応を返す
pub fn to_alpha_form(e: Expr) -> (Expr, TyMap) {
    let e = conv(Box::new(e), &mut Map::default());

    let mut tyenv = TyMap::default();
    make_tymap(&e, &mut tyenv);

    (*e, tyenv)
}
