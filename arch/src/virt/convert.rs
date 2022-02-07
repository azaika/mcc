use super::program::*;
use crate::common;
use ast::{closure, knormal::ConstKind};

use util::{Id, ToSpanned};

type ConstMap = util::Map<Id, ConstKind>;

fn gen_new_var(p: &mut Program, t: Ty) -> Id {
    let x = util::id::gen_tmp_var_with(t.short());
    p.tyenv.insert(x.clone(), t);
    x
}

fn conv(
    e: Box<closure::Expr>,
    p: &mut Program,
    tyenv: &closure::TyMap,
    consts: &ConstMap,
) -> Box<Expr> {
    use ExprKind::*;

    let lift = |e1: ExprKind| Box::new(e1.with_span(e.loc));
    match e.item {
        closure::ExprKind::Const(c) => match c {
            ConstKind::CUnit => lift(Nop),
            ConstKind::CInt(x) => lift(Li(x)),
            ConstKind::CFloat(x) => lift(FLi(x)),
        },
        closure::ExprKind::Var(x) => lift(Var(x)),
        closure::ExprKind::UnOp(kind, x) => match kind {
            ast::knormal::UnOpKind::Neg => lift(UnOp(UnOpKind::Neg, x)),
            ast::knormal::UnOpKind::FNeg => lift(UnOp(UnOpKind::FNeg, x)),
        },
        closure::ExprKind::BinOp(kind, x, y) => match kind {
            ast::knormal::BinOpKind::Add => lift(IntOp(IntOpKind::Add, x, Value::Var(y))),
            ast::knormal::BinOpKind::Sub => lift(IntOp(IntOpKind::Sub, x, Value::Var(y))),
            ast::knormal::BinOpKind::FAdd => lift(FloatOp(FloatOpKind::FAdd, x, y)),
            ast::knormal::BinOpKind::FSub => lift(FloatOp(FloatOpKind::FSub, x, y)),
            ast::knormal::BinOpKind::Mul => {
                if let Some(ConstKind::CInt(yi)) = consts.get(&y) {
                    if yi.count_ones() == 1 {
                        lift(IntOp(IntOpKind::Shl, x, Value::Imm(yi.log2() as i16)))
                    } else {
                        lift(IntOp(IntOpKind::Mul16, x, Value::Var(y)))
                    }
                } else {
                    lift(IntOp(IntOpKind::Mul16, x, Value::Var(y)))
                }
            }
            ast::knormal::BinOpKind::Div => {
                if let Some(ConstKind::CInt(yi)) = consts.get(&y) {
                    if yi.count_ones() == 1 {
                        lift(IntOp(IntOpKind::Shr, x, Value::Imm(yi.log2() as i16)))
                    } else {
                        panic!("division by non-pow2 value detected")
                    }
                } else {
                    panic!("division by non-pow2 value detected")
                }
            }
            ast::knormal::BinOpKind::FMul => lift(FloatOp(FloatOpKind::FMul, x, y)),
            ast::knormal::BinOpKind::FDiv => lift(FloatOp(FloatOpKind::FDiv, x, y)),
        },
        closure::ExprKind::If(kind, x, y, e1, e2) => {
            let t = tyenv.get(&x).unwrap();
            let kind = match t {
                Ty::Int | Ty::Float => match kind {
                    ast::knormal::IfKind::IfEq => IfKind::IfEq,
                    ast::knormal::IfKind::IfLE => IfKind::IfLE,
                },
                _ => panic!("comparison is supported only for bool, int, and float"),
            };

            let e1 = conv(e1, p, tyenv, consts);
            let e2 = conv(e2, p, tyenv, consts);

            match t {
                Ty::Int => lift(If(kind, x, Value::Var(y), e1, e2)),
                Ty::Float => lift(IfF(kind, x, y, e1, e2)),
                _ => unreachable!(),
            }
        }
        closure::ExprKind::Let(v, e1, e2) => {
            let e2 = conv(e2, p, tyenv, consts);

            let vt = tyenv.get(&v).unwrap();
            p.tyenv.insert(v.clone(), vt.clone());
            match &e1.item {
                closure::ExprKind::AllocArray(_, _, None) if !vt.is_pointer() => {
                    let e1 = lift(ExprKind::AllocHeap(Value::Imm(
                        common::type_size(&vt) as i16
                    )));
                    return lift(ExprKind::Let(Some(v), e1, e2));
                }
                closure::ExprKind::AllocArray(_, _, Some(init)) if !vt.is_pointer() => {
                    let (size, num) = match vt {
                        Ty::Array(t, size) => (common::type_size(t), *size),
                        _ => unreachable!(),
                    };

                    let mut e2 = e2;
                    for i in (0..num).rev() {
                        let offset = Value::Imm((i * size) as i16);
                        e2 = lift(ExprKind::Let(
                            None,
                            lift(ExprKind::Sw(v.clone(), offset, init.clone())),
                            e2,
                        ));
                    }

                    let e1 = lift(ExprKind::AllocHeap(Value::Imm(
                        common::type_size(&vt) as i16
                    )));
                    return lift(ExprKind::Let(Some(v), e1, e2));
                }
                closure::ExprKind::Load(label) if vt.is_pointer() => {
                    let e1 = lift(ExprKind::LoadLabel(label.clone()));
                    return lift(ExprKind::Let(Some(v), e1, e2));
                }
                closure::ExprKind::Load(label) => {
                    let e1 = lift(ExprKind::GetLabel(label.clone()));
                    return lift(ExprKind::Let(Some(v), e1, e2));
                }
                closure::ExprKind::MakeCls(label, args) => {
                    let offsets = match vt {
                        Ty::Fun(ts, _) => common::tuple_offsets(&Ty::Tuple(ts.clone())),
                        _ => unreachable!(),
                    };

                    let mut e2 = e2;
                    for (idx, x) in args.iter().enumerate().rev() {
                        e2 = lift(ExprKind::Let(
                            None,
                            lift(ExprKind::Sw(
                                v.clone(),
                                Value::Imm((idx + 1) as i16),
                                x.clone(),
                            )),
                            e2,
                        ));
                    }

                    let lab = gen_new_var(p, vt.clone());
                    let e2 = lift(ExprKind::Sw(v.clone(), Value::Imm(0), lab.clone()));
                    let e2 = lift(ExprKind::Let(Some(lab), lift(GetLabel(label.clone())), e2));

                    let e1 = lift(ExprKind::AllocHeap(Value::Imm(
                        (1 + offsets.last().unwrap()) as i16,
                    )));
                    return lift(ExprKind::Let(Some(v), e1, e2));
                }
                _ => (),
            };

            let e1 = conv(e1, p, tyenv, consts);
            lift(Let(Some(v), e1, e2))
        }
        closure::ExprKind::Tuple(xs) => {
            // `Tuple(x0, ..., x(n-1))` を
            // ```
            // let t = ALlocHeap(sizeof(Tuple(x0, ..., x(n-1)))) in
            // t.0 <- x0;
            // ...
            // t.(n-1) <- x(n-1);
            // t
            // ```
            // に変換
            let mut offsets = vec![];
            let mut acc: usize = 0;
            for x in &xs {
                offsets.push(acc);
                let s = common::type_size(tyenv.get(x).unwrap());
                acc += s;
            }
            let size = acc;

            let name = gen_new_var(
                p,
                Ty::Tuple(xs.iter().map(|x| tyenv.get(x).unwrap().clone()).collect()),
            );
            let mut e2 = lift(ExprKind::Var(name.clone()));
            for (offset, x) in offsets.into_iter().zip(xs).rev() {
                e2 = lift(ExprKind::Let(
                    None,
                    lift(ExprKind::Sw(name.clone(), Value::Imm(offset as i16), x)),
                    e2,
                ));
            }
            let e2 = lift(ExprKind::Let(
                Some(name),
                lift(ExprKind::AllocHeap(Value::Imm(size as i16))),
                e2,
            ));

            e2
        }
        closure::ExprKind::CallDir(label, args) => lift(CallDir(label, args)),
        closure::ExprKind::CallCls(cls, args) => lift(CallCls(cls, args)),
        closure::ExprKind::AllocArray(num, _, None) => lift(ExprKind::AllocHeap(Value::Var(num))),
        closure::ExprKind::AllocArray(num, t, Some(init)) => {
            let name = gen_new_var(p, Ty::ArrayPtr(Box::new(t)));
            let mut e2 = lift(ExprKind::Var(name.clone()));

            let num_i = if let Some(ConstKind::CInt(num)) = consts.get(&num) {
                if *num < 10 {
                    Some(*num)
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(num) = num_i {
                // 10 回以下なら展開
                for i in (0..num).rev() {
                    let offset = Value::Imm(i as i16); // Array of pointer or int or float
                    e2 = lift(ExprKind::Let(
                        None,
                        lift(ExprKind::Sw(name.clone(), offset, init.clone())),
                        e2,
                    ));
                }
            } else {
                let var = gen_new_var(p, Ty::Int);
                let end = gen_new_var(p, Ty::Int);

                let body = lift(ExprKind::If(
                    IfKind::IfLE,
                    var.clone(),
                    Value::Var(end.clone()),
                    lift(ExprKind::Sw(
                        name.clone(),
                        Value::Var(var.clone()),
                        init.clone(),
                    )),
                    lift(ExprKind::Nop),
                ));

                let lop = lift(ExprKind::Loop {
                    vars: vec![var.clone()],
                    init: vec![Value::Imm(0)],
                    body,
                });

                e2 = lift(ExprKind::Let(None, lop, e2));
                e2 = lift(ExprKind::Let(
                    Some(end),
                    lift(ExprKind::IntOp(IntOpKind::Sub, num.clone(), Value::Imm(1))),
                    e2,
                ));
            }

            lift(ExprKind::Let(
                Some(name),
                lift(ExprKind::AllocHeap(Value::Var(num))),
                e2,
            ))
        }
        closure::ExprKind::ExtArray(label) => lift(GetLabel(label)),
        closure::ExprKind::ArrayGet(arr, idx) => {
            let t = tyenv.get(&arr).unwrap().elem_t();
            let size = common::type_size(t) as i32;
            if (t.is_array() || t.is_tuple()) && !t.is_pointer() {
                if let Some(ConstKind::CInt(idx)) = consts.get(&idx) {
                    let offset = (*idx * size) as i16;
                    lift(ExprKind::IntOp(IntOpKind::Add, arr, Value::Imm(offset)))
                } else {
                    let offset_v = gen_new_var(p, Ty::Int);
                    let offset = lift(ExprKind::IntOp(
                        IntOpKind::Mul16,
                        idx,
                        Value::Imm(size as i16),
                    ));
                    let add = lift(ExprKind::IntOp(
                        IntOpKind::Add,
                        arr,
                        Value::Var(offset_v.clone()),
                    ));
                    lift(ExprKind::Let(Some(offset_v), offset, add))
                }
            } else {
                lift(ExprKind::Lw(arr, Value::Var(idx)))
            }
        }
        closure::ExprKind::ArrayPut(arr, idx, x) => lift(ExprKind::Sw(arr, Value::Var(idx), x)),
        closure::ExprKind::TupleGet(x, idx) => {
            let t = tyenv.get(&x).unwrap();
            let offsets = common::tuple_offsets(t);
            lift(ExprKind::Lw(x, Value::Imm(offsets[idx] as i16)))
        }
        closure::ExprKind::Loop { vars, init, body } => lift(ExprKind::Loop {
            vars,
            init: init.into_iter().map(|x| Value::Var(x)).collect(),
            body: conv(body, p, tyenv, consts),
        }),
        closure::ExprKind::DoAll {
            idx, range, body, ..
        } => lift(ExprKind::Loop {
            vars: vec![idx],
            init: vec![Value::Var(range.0)],
            body: conv(body, p, tyenv, consts),
        }),
        closure::ExprKind::Continue(ps) => lift(Continue(ps)),
        closure::ExprKind::Assign(label, x) => {
            let lab = gen_new_var(p, Ty::Tuple(vec![Ty::Int]));
            lift(ExprKind::Let(
                Some(lab.clone()),
                lift(GetLabel(label)),
                lift(ExprKind::Sw(lab.clone(), Value::Imm(0), x)),
            ))
        }
        closure::ExprKind::MakeCls(..) => panic!("found MakeCls left alone"),
        closure::ExprKind::Load(label) => panic!("found Load {label} left alone"),
        closure::ExprKind::Asm(inst, args) => {
            let k = if inst == "in" {
                ExprKind::In
            } else if inst == "out" {
                assert!(args.len() == 1);
                ExprKind::Out(args.into_iter().next().unwrap())
            } else if inst == "fabs" {
                assert!(args.len() == 1);
                ExprKind::UnOp(UnOpKind::FAbs, args.into_iter().next().unwrap())
            } else if inst == "fsqrt" {
                assert!(args.len() == 1);
                ExprKind::UnOp(UnOpKind::FSqrt, args.into_iter().next().unwrap())
            } else if inst == "ffloor" {
                assert!(args.len() == 1);
                ExprKind::UnOp(UnOpKind::FFloor, args.into_iter().next().unwrap())
            } else if inst == "fsin" {
                assert!(args.len() == 1);
                ExprKind::UnOp(UnOpKind::FSin, args.into_iter().next().unwrap())
            } else if inst == "fcos" {
                assert!(args.len() == 1);
                ExprKind::UnOp(UnOpKind::FCos, args.into_iter().next().unwrap())
            } else if inst == "fatan" {
                assert!(args.len() == 1);
                ExprKind::UnOp(UnOpKind::FAtan, args.into_iter().next().unwrap())
            } else if inst == "itof" {
                assert!(args.len() == 1);
                ExprKind::UnOp(UnOpKind::Itof, args.into_iter().next().unwrap())
            } else if inst == "ftoi" {
                assert!(args.len() == 1);
                ExprKind::UnOp(UnOpKind::Ftoi, args.into_iter().next().unwrap())
            } else {
                panic!("_asm\"`{inst}`\" is not supported")
            };

            lift(k)
        }
    }
}

fn concat_expr(mut e: Box<closure::Expr>, r: Box<closure::Expr>) -> Box<closure::Expr> {
    e.item = match e.item {
        closure::ExprKind::Const(ConstKind::CUnit) => return r,
        closure::ExprKind::Let(v, e1, e2) => closure::ExprKind::Let(v, e1, concat_expr(e2, r)),
        _ => e.item,
    };

    e
}

pub fn convert(cls: closure::Program, consts: &ConstMap) -> Program {
    let mut p = Program::new();
    let main = concat_expr(cls.global_init, cls.main);
    p.main = conv(main, &mut p, &cls.tyenv, consts);

    p.fundefs.reserve(cls.fundefs.len());
    for closure::Fundef {
        name,
        args,
        formal_fv,
        body,
    } in cls.fundefs
    {
        let body = conv(body, &mut p, &cls.tyenv, consts);
        p.fundefs.push(Fundef {
            name,
            args,
            formal_fv,
            body,
        });
    }

    p
}
