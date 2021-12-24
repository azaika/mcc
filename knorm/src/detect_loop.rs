use util::{Spanned, Id, id};

use ty::knormal::{ short, Ty };
use ast::knormal::*;

fn merge(p: (bool, bool), q: (bool, bool)) -> (bool, bool) {
    (p.0 || q.0, p.1 && q.1)
}

// returns: (is_called, is_tailrec)
fn is_tailrec(e: &Expr, f: &Id, is_inlet: bool) -> (bool, bool) {
    use ExprKind::*;
    match &e.item {
        Var(x) => (false, x != f),
        UnOp(_, x) => (false, x != f),
        BinOp(_, x, y) | CreateArray(x, y) | Get(x, y) => (false, x != f && y != f),
        If(_, x, y, e1, e2) if x != f && y != f => {
            merge(is_tailrec(&e1, f, is_inlet), is_tailrec(&e2, f, is_inlet))
        },
        Let(l) => {
            match l {
                LetKind::Let(_, e1, e2) => {
                    merge(is_tailrec(&e1, f, true), is_tailrec(&e2, f, is_inlet))
                },
                // when nested let rec found, assume that outer let rec is not tail recursive for simplicity
                LetKind::LetRec(_, _) => (false, false),
                LetKind::LetTuple(_, x, e2) => merge((false, x != f), is_tailrec(&e2, f, is_inlet)),
            }
        },
        Tuple(xs) | ExtApp(_, xs) | Continue(xs) => (false, xs.iter().all(|x| x != f)),
        App(func, args) => {
            if func == f {
                (true, !is_inlet && args.iter().all(|x| x != f))
            }
            else {
                (false, args.iter().all(|x| x != f))
            }
        },
        Put(x, y, z) => (false, x != f && y != f && z != f),
        Loop { init, body, .. } => {
            merge((false, init.iter().all(|x| x != f)), is_tailrec(&body, f, is_inlet))
        },
        _ => (false, true)
    }
}

fn insert_continue(mut e: Box<Expr>, f: &Id) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        App(func, args) if &func == f => Continue(args),
        If(kind, x, y, e1, e2) => If(kind, x, y, insert_continue(e1, f), insert_continue(e2, f)),
        Let(l) => Let(match l {
            LetKind::Let(d, e1, e2) => LetKind::Let(d, e1, insert_continue(e2, f)),
            LetKind::LetRec(_, _) => panic!(),
            LetKind::LetTuple(ds, x, e2) => LetKind::LetTuple(ds, x, insert_continue(e2, f)),
        }),
        Loop { vars, init, body } => Loop { vars, init, body: insert_continue(body, f) },
        _ => e.item
    };

    e
}

fn conv(mut e: Box<Expr>) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        If(kind, x, y, e1, e2) => If(kind, x, y, conv(e1), conv(e2)),
        Let(l) => Let(match l {
            LetKind::Let(d, e1, e2) => LetKind::Let(d, conv(e1), conv(e2)),
            LetKind::LetRec(Fundef { fvar, args, body }, e2) => {
                let (is_called, tailrec) = is_tailrec(&body, &fvar.name, false);
                if !is_called || !tailrec {
                    LetKind::LetRec(Fundef { fvar, args, body }, conv(e2))
                }
                else {
                    // 自己末尾再帰
                    // `let rec f x1 ... xn = e1 in e2` を
                    // ```
                    // let rec f x1' ... xn' =
                    //   let x1'' = Array.make(1, x1') in
                    //   ...
                    //   let xn'' = Array.make(1, xn') in
                    //   loop {
                    //     let x1 = x1''.(0) in
                    //     ...
                    //     let xn = xn''.(0) in
                    //     e1' (* e1 の自己再帰を continue に置換したもの *)
                    //   }
                    // in
                    // e2
                    // ```
                    // に変換する
                    log::info!("converting function `{}` to loop", fvar.name);

                    let loc = body.loc;
                    macro_rules! lift {
                        ($kind: expr) => {
                            Box::new(Spanned::new($kind, loc))
                        }
                    }
                    
                    let new_args: Vec<_> = args.iter().map(|d| Decl::new(id::distinguish(d.name.clone()), d.t.clone())).collect();
                    let vars: Vec<_> = args.iter().map(|d| Decl::new(id::distinguish(d.name.clone()), Ty::Array(Box::new(d.t.clone()), Some(1)))).collect();
                    let table: Vec<_> = args.into_iter().zip(&vars).map(|(d1, d2)| (d1, d2.name.clone())).collect();
                    let zero = id::gen_uniq_with(short(&Ty::Int));
                    let body = table.into_iter().rev().fold(
                        body,
                        |cont, (d, x)| {
                            lift!(Let(LetKind::Let(
                                d,
                                lift!(Get(x, zero.clone())),
                                cont
                            )))
                        }
                    );
                    let body = insert_continue(lift!(Let(LetKind::Let(
                        Decl::new(zero, Ty::Int),
                        lift!(Const(0.into())),
                        body
                    ))), &fvar.name);
                    let body = lift!(Loop { vars, init: new_args.iter().map(|d| d.name.clone()).collect(), body });
                    let fundef = Fundef {
                        fvar,
                        args: new_args,
                        body,
                    };
                    LetKind::LetRec(fundef, e2)
                }
            },
            LetKind::LetTuple(ds, x, e2) => LetKind::LetTuple(ds, x, conv(e2)),
        }),
        Loop { vars, init, body } => Loop { vars, init, body: conv(body) },
        _ => e.item
    };

    e
}

// 末尾自己再帰をループに変換する
pub fn detect_loop(e: Expr, tyenv: &mut TyMap) -> Expr {
    let e = *conv(Box::new(e));

    tyenv.clear();
    make_tymap(&e, tyenv);

    e
}