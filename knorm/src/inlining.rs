use util::Id;

use ast::knormal::*;

use crate::TyMap;

// (has_occurence, size)
type FuncInfo = (bool, usize);

fn merge(f1: FuncInfo, f2: FuncInfo) -> FuncInfo {
    (f1.0 || f2.0, f1.1 + f2.1)
}

// 式の中に変数 `name` が出現するかを判定する
// 関数呼び出しの中身までは見ない
fn calc_info(e: &Expr, name: &Id) -> FuncInfo {
    use ExprKind::*;
    match &e.item {
        Const(_) => (false, 1),
        Var(x) | UnOp(_, x) | ExtArray(x) | TupleGet(x, _) => (x == name, 1),
        BinOp(_, x, y) | CreateArray(x, y) | ArrayGet(x, y) => (x == name || y == name, 1),
        If(_, x, y, e1, e2) => {
            let (is_rec, size) = merge(calc_info(e1, name), calc_info(e2, name));
            (is_rec || x == name || y == name, size + 1)
        }
        Let(_, e1, e2)
        | LetRec(
            Fundef {
                fvar: _,
                args: _,
                body: e1,
            },
            e2,
        ) => {
            let (is_rec, size) = merge(calc_info(e1, name), calc_info(e2, name));
            (is_rec, size + 1)
        }
        Tuple(xs) => (xs.iter().any(|x| x == name), 1),
        App(f, args) | ExtApp(f, args) => (f == name || args.iter().any(|x| x == name), 1),
        ArrayPut(x, y, z) => ([x, y, z].iter().any(|x| *x == name), 1),
        Loop { init, body, .. } => {
            let (is_rec, size) = calc_info(body, name);
            (is_rec || init.iter().any(|x| x == name), size + 1)
        }
        Continue(xs) => (xs.iter().any(|(_, x)| x == name), 1),
    }
}

type Map = util::Map<Id, (Vec<Id>, Expr)>;

// 変換の呼び出し
fn conv(mut e: Box<Expr>, env: &mut Map, limit: usize) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        If(kind, x, y, e1, e2) => If(kind, x, y, conv(e1, env, limit), conv(e2, env, limit)),
        Let(decl, e1, e2) => Let(decl, conv(e1, env, limit), conv(e2, env, limit)),
        LetRec(Fundef { fvar, args, body }, e2) => {
            let (is_recursive, size) = calc_info(&body, &fvar.name);
            // 再帰関数や大きすぎる関数は展開しない
            if !is_recursive && size < limit {
                let xs = args.iter().map(|d| d.name.clone()).collect();
                env.insert(fvar.name.clone(), (xs, body.as_ref().clone()));
            }

            // 定義式の形を保存してから定義式を展開
            // 展開する前に定義式が膨張して展開されなくなるのを防止
            let body = conv(body, env, limit);
            LetRec(Fundef { fvar, args, body }, conv(e2, env, limit))
        }
        App(f, args) => {
            if let Some((ys, body)) = env.get(&f) {
                log::info!("inlining function `{}`.", f);

                let mut rename_map = util::Map::default();
                for (x, y) in args.into_iter().zip(ys) {
                    rename_map.insert(y.clone(), x);
                }

                return crate::alpha::conv(Box::new(body.clone()), &mut rename_map);
            } else {
                App(f, args)
            }
        }
        Loop { vars, init, body } => Loop {
            vars,
            init,
            body: conv(body, env, limit),
        },
        _ => e.item,
    };

    e
}

pub fn inlining(e: Expr, limit: usize, tyenv: &mut TyMap) -> Expr {
    let e = *conv(Box::new(e), &mut Map::default(), limit);
    tyenv.clear();
    make_tymap(&e, tyenv);
    e
}
