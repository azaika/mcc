pub mod error;
pub use error::*;

use std::{cell::RefCell, rc::Rc};
use log::info;

use ast::syntax::*;
use ty::Ty;

pub fn with(span: util::Span) -> impl FnOnce(UnifyError) -> TypeError {
    move |err: UnifyError| TypeError {
        expected: deref_ty(err.0),
        found: deref_ty(err.1),
        span
    }
}

type Map = util::Map<util::Id, Ty>;

fn check_occur(r: Rc<RefCell<Option<Ty>>>, t: &Ty) -> bool {
    use Ty::*;
    match t {
        Fun(a, t) => a.iter().any(|t| check_occur(r.clone(), t)) || check_occur(r, t),
        Tuple(a) => a.iter().any(|t| check_occur(r.clone(), t)),
        Array(t) => check_occur(r, t),
        Var(r1) => {
            if r.as_ptr() == r1.as_ptr() {
                return true;
            }
            match &*r1.borrow() {
                Some(r2) => check_occur(r, r2),
                None => false
            }
        },
        _ => false
    }
}

fn unify(t1 : &Ty, t2 : &Ty) -> Result<(), UnifyError> {
    if t1 == t2 {
        return Ok(());
    }

    use Ty::*;
    match (t1, t2) {
        (Fun(a1, r1), Fun(a2, r2)) => {
            if a1.len() != a2.len() {
                return Err(UnifyError(t1.clone(), t2.clone()));
            }
            for (t1, t2) in a1.iter().zip(a2) {
                unify(t1, t2)?;
            }
            unify(r1, r2)
        },
        (Tuple(a1), Tuple(a2)) => {
            if a1.len() != a2.len() {
                return Err(UnifyError(t1.clone(), t2.clone()));
            }
            for (t1, t2) in a1.iter().zip(a2) {
                unify(t1, t2)?;
            }
            Ok(())
        },
        (Array(t1), Array(t2)) => unify(t1, t2),
        (Var(r), t) | (t, Var(r)) => {
            if let Some(r) = &*r.borrow() {
                return unify(r, t);
            }
            
            if check_occur(r.clone(), t) {
                return Err(UnifyError(Var(r.clone()), t.clone()));
            }
            
            *r.borrow_mut() = Some(t.clone());
            Ok(())
        },
        (t1, t2) => Err(UnifyError(t1.clone(), t2.clone()))
    }
}

fn deref_ty(t: Ty) -> Ty {
    use Ty::*;
    match t {
        Fun(args, ret) => {
            Fun(args.into_iter().map(deref_ty).collect(), Box::new(deref_ty(*ret)))
        },
        Tuple(ts) => Tuple(ts.into_iter().map(deref_ty).collect()),
        Array(t) => Array(Box::new(deref_ty(*t))),
        Var(t) => {
            match t.take() {
                Some(t) => deref_ty(t),
                None => {
                    info!("uninstantiated type variable detected; assuming int.");
                    *t.borrow_mut() = Some(Int);
                    Var(Rc::new(RefCell::new(Some(Int))))
                }
            }
        },
        _ => t
    }
}

fn infer_impl(e : &Expr, env: &mut Map, extenv: &mut Map) -> Result<Ty, TypeError> {
    match &e.item {
        ExprKind::Const(c) => {
            use ConstKind::*;
            match c {
                CUnit => Ok(Ty::Unit),
                CBool(_) => Ok(Ty::Bool),
                CInt(_) => Ok(Ty::Int),
                CFloat(_) => Ok(Ty::Float)
            }
        },
        ExprKind::Var(v) => {
            if let Some(t) = env.get(v) {
                Ok(t.clone())
            }
            else if let Some(t) = extenv.get(v) {
                Ok(t.clone())
            }
            else {
                info!("free variable `{}` assumed as external.\n", v);
                let t = Ty::new_var();
                extenv.insert(v.clone(), t.clone());
                Ok(t)
            }
        },
        ExprKind::UnOp(op, e) => {
            use UnOpKind::*;
            let t = match op {
                Neg => Ty::Int,
                FNeg => Ty::Float,
                Not => Ty::Bool,
            };
            unify(&t.clone(), &infer_impl(e, env, extenv)?).map_err(with(e.loc))?;
            Ok(t)
        },
        ExprKind::BinOp(op, e1, e2) => {
            use BinOpKind::*;
            match op {
                Add | Sub | Mul | Div => {
                    unify(&Ty::Int, &infer_impl(e1, env, extenv)?).map_err(with(e1.loc))?;
                    unify(&Ty::Int, &infer_impl(e2, env, extenv)?).map_err(with(e2.loc))?;
                    Ok(Ty::Int)
                },
                FAdd | FSub | FMul | FDiv => {
                    unify(&Ty::Float, &infer_impl(e1, env, extenv)?).map_err(with(e1.loc))?;
                    unify(&Ty::Float, &infer_impl(e2, env, extenv)?).map_err(with(e2.loc))?;
                    Ok(Ty::Float)
                },
                Eq | LE => {
                    unify(&infer_impl(e1, env, extenv)?, &infer_impl(e2, env, extenv)?).map_err(with(e.loc))?;
                    Ok(Ty::Bool)
                }
            }
        },
        ExprKind::If(cond, e1, e2) => {
            unify(&Ty::Bool, &infer_impl(cond, env, extenv)?).map_err(with(cond.loc))?;
            let t1 = infer_impl(e1, env, extenv)?;
            let t2 = infer_impl(e2, env, extenv)?;
            unify(&t1, &t2).map_err(with(e.loc))?;
            Ok(t1.clone())
        },
        ExprKind::Let(l) => {
            use LetKind::*;
            match l {
                Let(decl, e1, e2) => {
                    unify(&decl.t, &infer_impl(e1, env, extenv)?).map_err(with(e1.loc))?;
                    env.insert(decl.name.clone(), decl.t.clone());
                    let r = infer_impl(e2, env, extenv);
                    env.remove(&decl.name);
                    r
                },
                LetRec(fundef, e) => {
                    let Decl { name, t } = &fundef.fvar;
                    env.insert(name.clone(), t.clone());
                    for Decl{ name, t} in &fundef.args {
                        env.insert(name.clone(), t.clone());
                    }

                    let fty = Ty::Fun(fundef.args.iter().map(|d| d.t.clone()).collect(), Box::new(infer_impl(&fundef.body, env, extenv)?));

                    unify(t, &fty).map_err(with(fundef.body.loc))?;

                    for d in &fundef.args {
                        env.remove(&d.name);
                    }

                    let r = infer_impl(e, env, extenv);
                    env.remove(name);
                    r
                },
                LetTuple(ds, e1, e2) => {
                    let t = Ty::Tuple(ds.iter().map(|x| x.t.clone()).collect());
                    unify(&t, &infer_impl(e1, env, extenv)?).map_err(with(e1.loc))?;

                    for Decl{ name, t} in ds {
                        env.insert(name.clone(), t.clone());
                    }
                    let r = infer_impl(e2, env, extenv);
                    for d in ds {
                        env.remove(&d.name);
                    }
                    r
                },
            }
        },
        ExprKind::Tuple(es) => {
            let ts = es.iter().map(|e| infer_impl(e, env, extenv)).collect::<Result<_, TypeError>>()?;
            Ok(Ty::Tuple(ts))
        },
        ExprKind::App(f, es) => {
            let rt = Ty::new_var();
            let ts = es.iter().map(|e| infer_impl(e, env, extenv)).collect::<Result<_, TypeError>>()?;
            
            let ft = Ty::Fun(ts, Box::new(rt.clone()));
            unify(&infer_impl(f, env, extenv)?, &ft).map_err(with(f.loc))?;
            
            Ok(rt)
        },
        ExprKind::Array(e1, e2) => {
            unify(&Ty::Int, &infer_impl(e1, env, extenv)?).map_err(with(e1.loc))?;
            Ok(Ty::Array(Box::new(infer_impl(e2, env, extenv)?)))
        },
        ExprKind::Get(e1, e2) => {
            let t = Ty::new_var();
            unify(&Ty::Array(Box::new(t.clone())), &infer_impl(e1, env, extenv)?).map_err(with(e1.loc))?;
            unify(&Ty::Int, &infer_impl(e2, env, extenv)?).map_err(with(e2.loc))?;
            Ok(t)
        },
        ExprKind::Put(e1, e2, e3) => {
            let t = infer_impl(e3, env, extenv)?;
            unify(&Ty::Array(Box::new(t.clone())), &infer_impl(e1, env, extenv)?).map_err(with(e1.loc))?;
            unify(&Ty::Int, &infer_impl(e2, env, extenv)?).map_err(with(e2.loc))?;
            Ok(Ty::Unit)
        },
    }
}

fn deref_decl(d: Decl) -> Decl {
    Decl {
        name: d.name,
        t: deref_ty(d.t)
    }
}

fn deref_term(e: Expr) -> Box<Expr> {
    use ExprKind::*;
    let ek = match e.item {
        UnOp(op, e) => UnOp(op, deref_term(*e)),
        BinOp(op, e1, e2) => BinOp(op, deref_term(*e1), deref_term(*e2)),
        If(e1, e2, e3) => If(deref_term(*e1), deref_term(*e2), deref_term(*e3)),
        Let(l) => {
            let l = match l {
                LetKind::Let(d, e1, e2) => {
                    LetKind::Let(deref_decl(d), deref_term(*e1), deref_term(*e2))
                },
                LetKind::LetRec(Fundef { fvar, args, body }, e) => {
                    LetKind::LetRec(
                        Fundef { fvar: deref_decl(fvar), args: args.into_iter().map(deref_decl).collect(), body: deref_term(*body) },
                        deref_term(*e)
                    )
                },
                LetKind::LetTuple(ds, e1, e2) => {
                    LetKind::LetTuple(ds.into_iter().map(deref_decl).collect(), deref_term(*e1), deref_term(*e2))
                },
            };
            Let(l)
        },
        Tuple(es) => Tuple(es.into_iter().map(|x| *deref_term(x)).collect()),
        App(f, args) => App(deref_term(*f), args.into_iter().map(|x|*deref_term(x)).collect()),
        Array(e1, e2) => Array(deref_term(*e1), deref_term(*e2)),
        Get(e1, e2) => Get(deref_term(*e1), deref_term(*e2)),
        Put(e1, e2, e3) => Put(deref_term(*e1), deref_term(*e2), deref_term(*e3)),
        e => e
    };
    Box::new(util::Spanned::new(ek, e.loc))
}

pub fn infer(mut e : Expr) -> Result<Expr, TypeError> {
    let mut extenv = Map::default();
    let t = infer_impl(&mut e, &mut Map::default(), &mut extenv)?;

    unify(&Ty::Unit, &t).map_err(with(e.loc))?;

    Ok(*deref_term(e))
}