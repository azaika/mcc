use util::{Spanned, Id};

use ast::knormal::*;

use crate::TyMap;

// (has_occurence, size)
type FuncInfo = (bool, usize);

fn merge(f1: FuncInfo, f2: FuncInfo) -> FuncInfo {
    (
        f1.0 || f2.0,
        f1.1 + f2.1,
    )
}

// 式の中に変数 `name` が出現するかを判定する
// 関数呼び出しの中身までは見ない
fn calc_info(e: &Expr, name: &Id) -> FuncInfo {
    match &e.item {
        ExprKind::Var(x) | ExprKind::UnOp(_, x) | ExprKind::ExtArray(x) => (x == name, 1),
        ExprKind::BinOp(_, x, y) | ExprKind::CreateArray(x, y) | ExprKind::Get(x, y) =>
            (x == name || y == name, 1),
        ExprKind::If(_, x, y, e1, e2) => {
            let info = merge(calc_info(e1, name), calc_info(e2, name));
            (x == name || y == name || info.0, info.1 + 1)
        },
        ExprKind::Let(l) => {
            match l {
                LetKind::Let(_, e1, e2)
                | LetKind::LetRec(Fundef { fvar: _, args: _, body: e1 }, e2) => {
                    let info = merge(calc_info(e1, name), calc_info(e2, name));
                    (info.0, info.1 + 1)
                },
                LetKind::LetTuple(_, x, e2) => {
                    let info = calc_info(e2, name);
                    (x == name || info.0, info.1 + 1)
                },
            }
        },
        ExprKind::Tuple(xs) => (xs.iter().any(|x| x == name), 1),
        ExprKind::App(f, args) | ExprKind::ExtApp(f, args)=>
            (f == name || args.iter().any(|x| x == name), 1),
        ExprKind::Put(x, y, z) => ([x, y, z].iter().any(|x| *x == name), 1),
        _ => (false, 1)
    }
}

type Map = util::Map<Id, (Vec<Id>, Expr)>;

// 変換の呼び出し
fn conv(e: Box<Expr>, env: &mut Map, limit: usize) -> Box<Expr> {
    use ExprKind::*;
    let kind = match e.item {
        If(kind, x, y, e1, e2) => If(kind, x, y, conv(e1, env, limit), conv(e2, env, limit)),
        Let(l) => {
            use LetKind::*;
            let kind = match l {
                Let(decl, e1, e2) => Let(decl, conv(e1, env, limit), conv(e2, env, limit)),
                LetRec(Fundef { fvar, args, body } , e2) => {
                    let body = conv(body, env, limit);
                    let (is_recursive, size) = calc_info(&body, &fvar.name);
                    // 再帰関数や大きすぎる関数は展開しない
                    if !is_recursive && size < limit {
                        let xs = args.iter().map(|d| d.name.clone()).collect();
                        env.insert(fvar.name.clone(), (xs, body.as_ref().clone()));
                    }
                    
                    LetRec(Fundef { fvar, args, body }, conv(e2, env, limit))
                },
                LetTuple(ds, x, e2) => LetTuple(ds, x, conv(e2, env, limit))
            };

            ExprKind::Let(kind)
        },
        App(f, args) => {
            if let Some((ys, body)) = env.get(&f) {
                log::info!("inlining function `{}`.", f);

                let mut rename_map = util::Map::default();
                for (x, y) in args.into_iter().zip(ys) {
                    rename_map.insert(y.clone(), x);
                }

                return rename(Box::new(body.clone()), &mut rename_map);
            }
            else {
                App(f, args)
            }
        },
        _ => return e
    };

    Box::new(Spanned::new(kind, e.loc))
}

pub fn inlining(e: Expr, limit: usize, tyenv: &mut TyMap) -> Expr {
    let e = *conv(Box::new(e), &mut Map::default(), limit);
    tyenv.clear();
    crate::alpha::make_tymap(&e, tyenv);
    e
}