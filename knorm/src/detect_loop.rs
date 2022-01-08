use bit_vec::BitVec;

use util::{Spanned, Id, id};
use ty::knormal::{ short, Ty };
use ast::knormal::*;

fn merge(p: (Option<BitVec>, bool), q: (Option<BitVec>, bool)) -> (Option<BitVec>, bool) {
    let b = match (p.0, q.0) {
        (None, None) => None,
        (x, None) | (None, x) => x,
        (Some(mut x), Some(y)) => { x.and(&y); Some(x) },
    };
    (
        b,
        p.1 && q.1
    )
}

// returns: (is_called, is_tailrec)
fn is_tailrec(e: &Expr, f: &Id, is_inlet: bool, orig: &Vec<Decl>) -> (Option<BitVec>, bool) {
    use ExprKind::*;
    match &e.item {
        Var(x) => (None, x != f),
        UnOp(_, x) => (None, x != f),
        BinOp(_, x, y) | CreateArray(x, y) | Get(x, y) => (None, x != f && y != f),
        If(_, x, y, e1, e2) => {
            if x != f && y != f {
                merge(is_tailrec(&e1, f, is_inlet, orig), is_tailrec(&e2, f, is_inlet, orig))
            }
            else {
                (None, false)
            }
        },
        Let(l) => {
            match l {
                LetKind::Let(_, e1, e2) => {
                    merge(is_tailrec(&e1, f, true, orig), is_tailrec(&e2, f, is_inlet, orig))
                },
                // when nested let rec found, assume that outer let rec is not tail recursive (for simplicity)
                LetKind::LetRec(_, _) => (None, false),
                LetKind::LetTuple(_, x, e2) => merge((None, x != f), is_tailrec(&e2, f, is_inlet, orig)),
            }
        },
        Tuple(xs) | ExtApp(_, xs) => (None, xs.iter().all(|x| x != f)),
        App(func, args) => {
            if func == f {
                let b = args.iter().zip(orig).map(|(x, y)| x == &y.name).collect();
                (Some(b), !is_inlet && args.iter().all(|x| x != f))
            }
            else {
                (None, args.iter().all(|x| x != f))
            }
        },
        Put(x, y, z) => (None, x != f && y != f && z != f),
        Loop { init, body, .. } => {
            merge((None, init.iter().all(|x| x != f)), is_tailrec(&body, f, is_inlet, orig))
        },
        Continue(xs) => (None, xs.iter().all(|(_, x)| x != f)),
        _ => (None, true)
    }
}

fn insert_continue(mut e: Box<Expr>, f: &Id, orig: &Vec<Decl>, mask: &BitVec) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        App(func, args) if &func == f => {
            let args = args.into_iter().enumerate().filter_map(|(idx, x)|
                if mask[idx] { None } else { Some((orig[idx].name.clone(), x)) }
            ).collect();
            Continue(args)
        },
        If(kind, x, y, e1, e2) => If(kind, x, y, insert_continue(e1, f, orig,  mask), insert_continue(e2, f, orig, mask)),
        Let(l) => Let(match l {
            LetKind::Let(d, e1, e2) => LetKind::Let(d, e1, insert_continue(e2, f, orig, mask)),
            LetKind::LetRec(_, _) => panic!(),
            LetKind::LetTuple(ds, x, e2) => LetKind::LetTuple(ds, x, insert_continue(e2, f, orig, mask)),
        }),
        Loop { vars, init, body } => Loop { vars, init, body: insert_continue(body, f, orig, mask) },
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
                let (called, tailrec) = is_tailrec(&body, &fvar.name, false, &args);
                if called.is_none() || !tailrec {
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

                    let mask = called.unwrap();
                    assert!(mask.len() == args.len());

                    let loc = body.loc;
                    macro_rules! lift {
                        ($kind: expr) => {
                            Box::new(Spanned::new($kind, loc))
                        }
                    }

                    let mut body = insert_continue(body, &fvar.name, &args, &mask);

                    let mut new_args = vec![];
                    let mut init = vec![];
                    let mut vars = vec![];
                    let zero = id::gen_uniq_with(short(&Ty::Int));
                    for (idx, d) in args.into_iter().enumerate().rev() {
                        if mask[idx] {
                            new_args.push(d);
                        }
                        else {
                            let x = id::distinguish(d.name.clone());
                            new_args.push(Decl::new(x.clone(), d.t.clone()));
                            init.push(x);
                            let v = id::distinguish(d.name.clone());
                            let vt = Ty::Array(Box::new(d.t.clone()), Some(1));
                            vars.push(Decl::new(v.clone(), vt));

                            body = lift!(Let(LetKind::Let(
                                Decl::new(d.name, d.t),
                                lift!(Get(v, zero.clone())),
                                body
                                )));
                        }
                    }
                    new_args.reverse();
                    init.reverse();
                    vars.reverse();

                    let body = lift!(Let(LetKind::Let(
                        Decl::new(zero, Ty::Int),
                        lift!(Const(0.into())),
                        body
                    )));
                    let body = lift!(Loop { vars, init, body });
                    let fundef = Fundef {
                        fvar,
                        args: new_args,
                        body,
                    };
                    LetKind::LetRec(fundef, conv(e2))
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