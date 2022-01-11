use anyhow::Context;
use anyhow::Result;

use ast::{syntax, knormal};
use ty::knormal::Ty;
use ty::syntax::Ty as SyntaxTy;
use util::Id;
use util::Map as FnvMap;
use util::Spanned;
use knormal::*;

type Map = FnvMap<Id, Ty>;
type SyntaxMap = FnvMap<Id, SyntaxTy>;

fn decl_conv(decl: syntax::Decl) -> Decl {
    Decl { name: decl.name, t: decl.t.into() }
}

fn insert_let<F: FnOnce(Id) -> (ExprKind, Ty)>(e: Expr, t: Ty, loc: util::Span, k: F) -> (ExprKind, Ty) {
    match e.item {
        ExprKind::Var(x) => k(x),
        _ => {
            let x = util::id::gen_uniq_with(t.short());
            let (e2, t2) = k(x.clone());
            let kind = LetKind::Let(Decl::new(x, t), Box::new(e), Box::new(Spanned::new(e2, loc)));
            (ExprKind::Let(kind), t2)
        }
    }
}

fn is_comparator(op: &syntax::BinOpKind) -> bool {
    match op {
        syntax::BinOpKind::Eq | syntax::BinOpKind::LE => true,
        _ => false
    }
}

fn conv(e: Box<syntax::Expr>, env: &mut Map, extenv: &SyntaxMap) -> Result<(Box<knormal::Expr>, Ty)> {
    macro_rules! lift {
        ($kind: expr) => {
            Box::new(Spanned::new($kind, e.loc))
        }
    }

    let (kind, t) = match e.item {
        syntax::ExprKind::Const(c) => {
            let (kind, t) = match c {
                syntax::ConstKind::CUnit => (ConstKind::CUnit, Ty::Unit),
                syntax::ConstKind::CBool(b) => ((if b {1i32} else {0}).into(), Ty::Int),
                syntax::ConstKind::CInt(i) => (i.into(), Ty::Int),
                syntax::ConstKind::CFloat(f) => (f.into(), Ty::Float),
            };
            (ExprKind::Const(kind), t)
        },
        syntax::ExprKind::Var(x) => {
            if let Some(t) = env.get(&x) {
                (ExprKind::Var(x), t.clone())
            }
            else {
                // 外部配列の参照
                let t = extenv.get(&x).unwrap().clone().into();
                if let Ty::Array(_) = t {
                    (ExprKind::ExtArray(x), t)
                }
                else {
                    return Err(anyhow::Error::msg(format!("external variable `{}` must have an array type, but it has type `{}`", x, t)));
                }
            }
        },
        syntax::ExprKind::UnOp(syntax::UnOpKind::Not, e) => {
            // if {e} then false else true に変換
            return conv(
                lift!(syntax::ExprKind::If(
                    e,
                    lift!(syntax::ExprKind::Const(false.into())),
                    lift!(syntax::ExprKind::Const(true.into())))
                ),
                env,
                extenv
            );
        },
        syntax::ExprKind::UnOp(op, e_) => {
            let (t, op) = match op {
                syntax::UnOpKind::Neg => (Ty::Int, UnOpKind::Neg),
                syntax::UnOpKind::FNeg => (Ty::Float, UnOpKind::FNeg),
                _ => unreachable!()
            };

            let (e_, t_) = conv(e_, env, extenv)?;

            insert_let(*e_, t_, e.loc, |x| (ExprKind::UnOp(op, x), t))
        },
        syntax::ExprKind::BinOp(op, e1, e2) if is_comparator(&op)=> {
            // if {e} then true else false に変換
            return conv(
                lift!(syntax::ExprKind::If(
                    lift!(syntax::ExprKind::BinOp(op, e1, e2)),
                    lift!(syntax::ExprKind::Const(true.into())),
                    lift!(syntax::ExprKind::Const(false.into())))
                ),
                env,
                extenv
            );
        },
        syntax::ExprKind::BinOp(op, e1, e2) => {
            let (t, op) = match op {
                syntax::BinOpKind::Add => (Ty::Int, BinOpKind::Add),
                syntax::BinOpKind::Sub => (Ty::Int, BinOpKind::Sub),
                syntax::BinOpKind::FAdd => (Ty::Float, BinOpKind::FAdd),
                syntax::BinOpKind::FSub => (Ty::Float, BinOpKind::FSub),
                syntax::BinOpKind::Mul => (Ty::Int, BinOpKind::Mul),
                syntax::BinOpKind::Div => (Ty::Int, BinOpKind::Div),
                syntax::BinOpKind::FMul => (Ty::Float, BinOpKind::FMul),
                syntax::BinOpKind::FDiv => (Ty::Float, BinOpKind::FDiv),
                _ => unreachable!()
            };

            let (e1, t1) = conv(e1, env, extenv)?;
            let (e2, t2) = conv(e2, env, extenv)?;

            insert_let(*e1, t1, e.loc, |x|
                insert_let(*e2, t2, e.loc, |y|
                    (ExprKind::BinOp(op, x, y), t)
                )
            )
        },
        syntax::ExprKind::If(cond, e1, e2) => {
            match cond.item {
                syntax::ExprKind::BinOp(op, e3, e4) if is_comparator(&op) => {
                    let kind = match op {
                        syntax::BinOpKind::Eq => IfKind::IfEq,
                        syntax::BinOpKind::LE => IfKind::IfLE,
                        _ => unreachable!()
                    };
                    let (e1, t) = conv(e1, env, extenv)?;
                    let (e2, _) = conv(e2, env, extenv)?;
                    let (e3, t3) = conv(e3, env, extenv)?;
                    let (e4, t4) = conv(e4, env, extenv)?;

                    insert_let(*e3, t3, e.loc, |x|
                        insert_let(*e4, t4, e.loc, |y|
                            (ExprKind::If(kind, x, y, e1, e2), t)
                        )
                    )
                },
                syntax::ExprKind::UnOp(syntax::UnOpKind::Not, e3) => {
                    return conv(
                        lift!(syntax::ExprKind::If(e3, e2, e1)),
                        env,
                        extenv
                    );
                },
                _ => {
                    // if {cond} = false then e2 else e1 に変換
                    // {cond} = false なのは 0 の分岐は最適化できうるから
                    return conv(
                        lift!(syntax::ExprKind::If(
                            lift!(syntax::ExprKind::BinOp(
                                syntax::BinOpKind::Eq,
                                cond,
                                lift!(syntax::ExprKind::Const(false.into()))
                            )),
                            e2,
                            e1
                        )),
                        env,
                        extenv
                    );
                }
            } 
        },
        syntax::ExprKind::Let(l) => {
            fn restore(m: &mut Map, key: &str, t: Option<Ty>) {
                match t {
                    Some(t) => m.insert(key.to_string(), t),
                    None => m.remove(key)
                };
            }

            match l {
                syntax::LetKind::Let(decl, e1, e2) => {
                    let (e1, _) = conv(e1, env, extenv)?;

                    let s = env.insert(decl.name.clone(), decl.t.clone().into());
                    let (e2, t2) =  conv(e2, env, extenv)?;

                    restore(env, &decl.name, s);
                    
                    (ExprKind::Let(LetKind::Let(decl_conv(decl), e1, e2)), t2)
                },
                syntax::LetKind::LetRec(fundef, e2) => {
                    let decl = fundef.fvar;

                    let old_f = env.insert(decl.name.clone(), decl.t.clone().into());

                    let (e2, t2) =  conv(e2, env, extenv)?;

                    let mut old_args = vec![];
                    for syntax::Decl{ name, t} in &fundef.args {
                        old_args.push(env.insert(name.clone(), t.clone().into()));
                    }

                    let (body, _) =  conv(fundef.body, env, extenv)?;

                    for (t, d) in old_args.into_iter().zip(&fundef.args) {
                        restore(env, &d.name, t);
                    }
                    restore(env, &decl.name, old_f);

                    let fundef = Fundef {
                        fvar: decl_conv(decl),
                        args: fundef.args.into_iter().map(decl_conv).collect(),
                        body
                    };

                    (ExprKind::Let(LetKind::LetRec(fundef, e2)), t2)
                },
                syntax::LetKind::LetTuple(ds, e1, e2) => {
                    let (e1, t1) = conv(e1, env, extenv)?;

                    let mut old_vars = vec![];
                    for syntax::Decl{ name, t} in &ds {
                        old_vars.push(env.insert(name.clone(), t.clone().into()));
                    }

                    let (e2, t2) = conv(e2, env, extenv)?;

                    for (t, d) in old_vars.into_iter().zip(&ds) {
                        restore(env, &d.name, t);
                    }

                    // `let (x1, ..., xn) = x in e2` を
                    // ```
                    // let x1 = x.0 in
                    // ...
                    // let xn = x.(n-1) in
                    // e2
                    // ```
                    // に変換
                    insert_let(*e1, t1, e.loc, |x| {
                        let mut e2 = e2;
                        for (idx, d) in ds.into_iter().enumerate() {
                            e2 = lift!(ExprKind::Let(LetKind::Let(
                                decl_conv(d),
                                lift!(ExprKind::TupleGet(x.clone(), idx)),
                                e2
                            )));
                        }

                        (e2.item, t2)
                    })
                },
            }
        },
        syntax::ExprKind::Tuple(es) => {
            assert!(!es.is_empty());
            let n = es.len();
            let mut conv_es = vec![];
            conv_es.reserve(n);
            for e in es {
                conv_es.push(conv(Box::new(e), env, extenv)?);
            }
            let es = conv_es;

            // ```
            // let v1 = e1 in
            // let v2 = e2 in
            // ...
            // let vn = en in
            // (v1, ..., vn)
            // ```
            // に変換

            // let xi: ti = ei として xs = vec{xi}, ts = vec{ti}
            let make_tuple = Box::new(|vs, ts| {
                (ExprKind::Tuple(vs), Ty::Tuple(ts))
            }) as Box<dyn FnOnce(Vec<Id>, Vec<Ty>) -> (ExprKind, Ty)>;
            es.into_iter().rev().fold(
                make_tuple,
                |k, (ei, ti)| Box::new(|mut vs, mut ts| {
                    insert_let(*ei, ti.clone(), e.loc, |xi| {
                        vs.push(xi);
                        ts.push(ti);
                        k(vs, ts)
                    })
                })
            )(Vec::with_capacity(n), Vec::with_capacity(n))
        },
        syntax::ExprKind::App(f, args) => {
            assert!(!args.is_empty());
            let n = args.len();
            let mut conv_args = vec![];
            conv_args.reserve(n);
            for arg in args {
                conv_args.push(conv(Box::new(arg), env, extenv)?);
            }
            let args = conv_args;

            // おおまかには Tuple のときと同じだが, 関数側が外部関数かどうかで場合分け
            let make_app = match &f.item {
                syntax::ExprKind::Var(x) if extenv.contains_key(x) => {
                    // 外部関数の呼び出し
                    let tf = extenv.get(x).unwrap();
                    if let SyntaxTy::Fun(_, t) = tf {
                        let t = (**t).clone().into();
                        Box::new(|xs| {
                            (ExprKind::ExtApp(x.clone(), xs), t)
                        }) as Box<dyn FnOnce(Vec<Id>) -> (ExprKind, Ty)>
                    }
                    else {
                        return Err(anyhow::Error::msg(format!("external variable `{}` must have an function type, but it has type `{}`", x, tf)));
                    }
                },
                _ => {
                    let (f, tf) = conv(f, env, extenv)?;
                    let t = match &tf {
                        Ty::Fun(_, t) => (**t).clone(),
                        _ => panic!()
                    };

                    Box::new(|xs| {
                        insert_let(*f, tf, e.loc, |x| (ExprKind::App(x.clone(), xs), t))
                    }) as Box<dyn FnOnce(Vec<Id>) -> (ExprKind, Ty)>
                }
            };

            args.into_iter().rev().fold(
                make_app,
                |k, (ei, ti)| Box::new(|mut vs| {
                    insert_let(*ei, ti, e.loc, |xi| {
                        vs.push(xi);
                        k(vs)
                    })
                })
            )(Vec::with_capacity(n))
        },
        syntax::ExprKind::Array(e1, e2) => {
            let (e1, t1) = conv(e1, env, extenv)?;
            let (e2, t2) = conv(e2, env, extenv)?;

            let t = Ty::Array(Box::new(t2.clone().into()));

            insert_let(*e1, t1, e.loc, |idx|
                insert_let(*e2, t2, e.loc, |init|
                    (ExprKind::CreateArray(idx, init), t)
                )
            )
        },
        syntax::ExprKind::Get(e1, e2) => {
            let (e1, t1) = conv(e1, env, extenv)?;
            let (e2, t2) = conv(e2, env, extenv)?;

            let t = match &t1 {
                Ty::Array(t) => (**t).clone(),
                _ => panic!()
            };

            insert_let(*e1, t1, e.loc, |arr|
                insert_let(*e2, t2, e.loc, |idx|
                    (ExprKind::ArrayGet(arr, idx), t)
                )
            )
        },
        syntax::ExprKind::Put(e1, e2, e3) => {
            let (e1, t1) = conv(e1, env, extenv)?;
            let (e2, t2) = conv(e2, env, extenv)?;
            let (e3, t3) = conv(e3, env, extenv)?;

            let t = t3.clone();

            insert_let(*e1, t1, e.loc, |arr|
                insert_let(*e2, t2, e.loc, |idx|
                    insert_let(*e3, t3, e.loc, |v|
                        (ExprKind::ArrayPut(arr, idx, v), t)
                    )
                )
            )
        }
    };

    Ok((Box::new(Spanned::new(kind, e.loc)), t))
}

pub fn convert(e: syntax::Expr, extenv: &SyntaxMap) -> Result<knormal::Expr> {
    conv(Box::new(e), &mut Map::default(), extenv)
        .map(|x| *x.0)
        .context("error occurred in converting process to KNormal form")
}