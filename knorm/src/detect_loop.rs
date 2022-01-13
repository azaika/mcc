use bit_vec::BitVec;

use util::{Spanned, Id, id};
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
        BinOp(_, x, y) | CreateArray(x, y) | ArrayGet(x, y) => (None, x != f && y != f),
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
        ArrayPut(x, y, z) => (None, x != f && y != f && z != f),
        Loop { init, body, .. } => {
            if init.iter().all(|x| x != f) {
                let t = is_tailrec(&body, f, is_inlet, orig);
                if t.0.is_some() {
                    // if self-recursive in Loop, this function is not tail-recursive
                    (None, false)
                }
                else {
                    (None, true)
                }
            }
            else {
                (None, false)
            }
        },
        Continue(xs) => (None, xs.iter().all(|(_, x)| x != f)),
        _ => (None, true)
    }
}

fn insert_continue(mut e: Box<Expr>, f: &Id, loop_vars: &Vec<Decl>, mask: &BitVec) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        App(func, args) if &func == f => {
            let mut vars_idx = 0;
            let args = args.into_iter().enumerate().filter_map(|(idx, x)|
                if mask[idx] {
                    None
                }
                else {
                    let r = Some((loop_vars[vars_idx].name.clone(), x));
                    vars_idx += 1;
                    r
                }
            ).collect();
            Continue(args)
        },
        If(kind, x, y, e1, e2) => If(kind, x, y, insert_continue(e1, f, loop_vars,  mask), insert_continue(e2, f, loop_vars, mask)),
        Let(l) => Let(match l {
            LetKind::Let(d, e1, e2) => LetKind::Let(d, e1, insert_continue(e2, f, loop_vars, mask)),
            LetKind::LetRec(_, _) => panic!(),
        }),
        Loop { .. } => {
            // double loops CANNOT include self-recursion in this context
            e.item
        },
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
                    //   let x1 = Mutable(x1') in
                    //   ...
                    //   let xn = Mutable(xn') in
                    //   loop {
                    //     e1' (* e1 の自己再帰を continue に置換したもの *)
                    //   }
                    // in
                    // e2
                    // ```
                    // に変換する
                    log::debug!("converting function `{}` to loop", fvar.name);

                    let mask = called.unwrap();
                    assert!(mask.len() == args.len());

                    let loc = body.loc;
                    macro_rules! lift {
                        ($kind: expr) => {
                            Box::new(Spanned::new($kind, loc))
                        }
                    }

                    let mut new_args = vec![];
                    let mut init = vec![];
                    let mut vars = vec![];
                    for (idx, d) in args.into_iter().enumerate() {
                        if mask[idx] {
                            new_args.push(d);
                        }
                        else {
                            let x = id::distinguish(d.name.clone());
                            new_args.push(Decl::new(x.clone(), d.t.clone()));
                            init.push(x);
                            vars.push(d);
                        }
                    }

                    let body = insert_continue(body, &fvar.name, &vars, &mask);
                    let body = lift!(Loop { vars, init, body });
                    let fundef = Fundef {
                        fvar,
                        args: new_args,
                        body,
                    };
                    LetKind::LetRec(fundef, conv(e2))
                }
            },
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