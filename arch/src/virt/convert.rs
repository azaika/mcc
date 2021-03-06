use super::program::*;
use crate::common::{self, type_size};
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
                        common::type_size(&vt).try_into().unwrap(),
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
                        let offset = Value::Imm((i * size).try_into().unwrap());
                        e2 = lift(ExprKind::Let(
                            None,
                            lift(ExprKind::Sw(v.clone(), offset, init.clone())),
                            e2,
                        ));
                    }

                    let e1 = lift(ExprKind::AllocHeap(Value::Imm(
                        common::type_size(&vt).try_into().unwrap(),
                    )));
                    return lift(ExprKind::Let(Some(v), e1, e2));
                }
                closure::ExprKind::Load(label)
                    if (vt.is_array() || vt.is_tuple()) && !vt.is_pointer() =>
                {
                    let e1 = lift(ExprKind::GetLabel(label.clone()));
                    return lift(ExprKind::Let(Some(v), e1, e2));
                }
                closure::ExprKind::Load(label) => {
                    let e1 = lift(ExprKind::LoadLabel(label.clone()));
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
                                Value::Imm((idx + 1).try_into().unwrap()),
                                x.clone(),
                            )),
                            e2,
                        ));
                    }

                    let lab = gen_new_var(p, vt.clone());
                    let e2 = lift(ExprKind::Sw(v.clone(), Value::Imm(0), lab.clone()));
                    let e2 = lift(ExprKind::Let(Some(lab), lift(GetLabel(label.clone())), e2));

                    let e1 = lift(ExprKind::AllocHeap(Value::Imm(
                        (1 + offsets.last().unwrap()).try_into().unwrap(),
                    )));
                    return lift(ExprKind::Let(Some(v), e1, e2));
                }
                _ => (),
            };

            let e1 = conv(e1, p, tyenv, consts);
            lift(Let(Some(v), e1, e2))
        }
        closure::ExprKind::Tuple(xs) => {
            // `Tuple(x0, ..., x(n-1))` ???
            // ```
            // let t = ALlocHeap(sizeof(Tuple(x0, ..., x(n-1)))) in
            // t.0 <- x0;
            // ...
            // t.(n-1) <- x(n-1);
            // t
            // ```
            // ?????????
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
                    lift(ExprKind::Sw(
                        name.clone(),
                        Value::Imm(offset.try_into().unwrap()),
                        x,
                    )),
                    e2,
                ));
            }
            let e2 = lift(ExprKind::Let(
                Some(name),
                lift(ExprKind::AllocHeap(Value::Imm(size.try_into().unwrap()))),
                e2,
            ));

            e2
        }
        closure::ExprKind::CallDir(label, args) => {
            let args = args.into_iter().map(|x| Value::Var(x)).collect();
            lift(CallDir(label, args))
        }
        closure::ExprKind::CallCls(cls, args) => {
            let args = args.into_iter().map(|x| Value::Var(x)).collect();
            lift(CallCls(cls, args))
        }
        closure::ExprKind::AllocArray(num, t, None) => {
            let st = type_size(&t);
            if st == 1 {
                lift(ExprKind::AllocHeap(Value::Var(num)))
            } else {
                let s = gen_new_var(p, Ty::Int);
                let alloc = lift(ExprKind::AllocHeap(Value::Var(s.clone())));
                let size = lift(ExprKind::IntOp(
                    IntOpKind::Mul16,
                    num,
                    Value::Imm(st.try_into().unwrap()),
                ));
                lift(ExprKind::Let(Some(s), size, alloc))
            }
        }
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
                // 10 ?????????????????????
                for i in (0..num).rev() {
                    let offset = Value::Imm(i.try_into().unwrap()); // Array of pointer or int or float
                    e2 = lift(ExprKind::Let(
                        None,
                        lift(ExprKind::Sw(name.clone(), offset, init.clone())),
                        e2,
                    ));
                }
            } else {
                let var = gen_new_var(p, Ty::Int);
                let end = gen_new_var(p, Ty::Int);
                let v_next = gen_new_var(p, Ty::Int);

                let body = lift(ExprKind::Let(
                    Some(v_next.clone()),
                    lift(ExprKind::IntOp(IntOpKind::Add, var.clone(), Value::Imm(1))),
                    lift(ExprKind::Continue(vec![(var.clone(), v_next.clone())])),
                ));
                let body = lift(ExprKind::Let(
                    None,
                    lift(ExprKind::Sw(
                        name.clone(),
                        Value::Var(var.clone()),
                        init.clone(),
                    )),
                    body,
                ));

                let body = lift(ExprKind::If(
                    IfKind::IfLE,
                    var.clone(),
                    Value::Var(end.clone()),
                    body,
                    lift(ExprKind::Nop),
                ));

                let lop = lift(ExprKind::Loop {
                    vars: vec![var.clone()],
                    init: vec![Value::Imm(0)],
                    body,
                });

                e2 = lift(ExprKind::Let(None, lop, e2));

                let end_def = if let Some(ConstKind::CInt(num)) = consts.get(&num) {
                    ExprKind::Li(*num - 1)
                } else {
                    ExprKind::IntOp(IntOpKind::Sub, num.clone(), Value::Imm(1))
                };
                e2 = lift(ExprKind::Let(Some(end), lift(end_def), e2));
            }

            lift(ExprKind::Let(
                Some(name),
                lift(ExprKind::AllocHeap(Value::Var(num))),
                e2,
            ))
        }
        closure::ExprKind::ExtArray(label) => lift(GetLabel(label)),
        closure::ExprKind::ArrayGet(arr, idx) => {
            let t = tyenv.get(&arr).unwrap();
            let et = t.elem_t();
            let size: i32 = common::type_size(et).try_into().unwrap();
            if !et.is_pointer() && (et.is_array() || et.is_tuple()) {
                if let Some(ConstKind::CInt(idx)) = consts.get(&idx) {
                    let offset = (*idx * size).try_into().unwrap();
                    lift(ExprKind::IntOp(IntOpKind::Add, arr, Value::Imm(offset)))
                } else if size == 1 {
                    lift(ExprKind::IntOp(IntOpKind::Add, arr, Value::Var(idx)))
                } else {
                    let offset_v = gen_new_var(p, Ty::Int);
                    let offset = lift(ExprKind::IntOp(
                        IntOpKind::Mul16,
                        idx,
                        Value::Imm(size.try_into().unwrap()),
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
            lift(ExprKind::Lw(
                x,
                Value::Imm(offsets[idx].try_into().unwrap()),
            ))
        }
        closure::ExprKind::Loop { vars, init, body } => {
            for v in &vars {
                let t = tyenv.get(v).unwrap().clone();
                p.tyenv.insert(v.clone(), t);
            }
            lift(ExprKind::Loop {
                vars,
                init: init.into_iter().map(|x| Value::Var(x)).collect(),
                body: conv(body, p, tyenv, consts),
            })
        }
        closure::ExprKind::DoAll {
            idx, range, body, ..
        } => {
            let t = tyenv.get(&idx).unwrap().clone();
            p.tyenv.insert(idx.clone(), t);

            lift(ExprKind::Loop {
                vars: vec![idx],
                init: vec![Value::Var(range.0)],
                body: conv(body, p, tyenv, consts),
            })
        }
        closure::ExprKind::Continue(ps) => lift(Continue(ps)),
        closure::ExprKind::Assign(label, x) => {
            if consts.get(&x).is_none() {
                // const global ????????????????????????????????????????????????????????????
                let lab = gen_new_var(p, Ty::Tuple(vec![Ty::Int]));
                lift(ExprKind::Let(
                    Some(lab.clone()),
                    lift(GetLabel(label)),
                    lift(ExprKind::Sw(lab.clone(), Value::Imm(0), x)),
                ))
            } else {
                // const global ???????????? assign ????????????????????????
                lift(Nop)
            }
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
    for g in cls.globals {
        let t = cls.tyenv.get(&g).unwrap();
        let s = common::type_size(t);

        use GlobalData::*;
        let data = match consts.get(&g).cloned() {
            Some(ConstKind::CInt(i)) => {
                if let Ok(i) = i.try_into() {
                    GInt16(i)
                } else {
                    GInt32(i)
                }
            }
            Some(ConstKind::CFloat(x)) => GFloat(x),
            Some(ConstKind::CUnit) | None => GSpace(s),
        };

        p.globals.push((g, data));
    }

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
        let ft = cls.tyenv.get(&name).unwrap().clone();
        p.tyenv.insert(name.clone(), ft);
        for x in args.iter().chain(&formal_fv) {
            let t = cls.tyenv.get(x).unwrap().clone();
            p.tyenv.insert(x.clone(), t);
        }

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
