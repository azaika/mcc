use super::program::*;
use util::{Id, Map};

fn conv(
    mut e: Box<Expr>,
    global_int: &Map<Id, i32>,
    global_float: &Map<Id, f32>,
    env: &mut Map<Id, i16>,
) -> Box<Expr> {
    use ExprKind::*;

    macro_rules! map {
        ($name: expr) => {
            match $name {
                Value::Var(x) => {
                    if let Some(x) = env.get(&x) {
                        Value::Imm(*x)
                    } else {
                        Value::Var(x)
                    }
                }
                x => x,
            }
        };
    }
    e.item = match e.item {
        Var(x) => {
            if let Some(x) = env.get(&x) {
                Li(*x as i32)
            } else if let Some(x) = global_int.get(&x) {
                Li(*x)
            } else if let Some(x) = global_float.get(&x) {
                FLi(*x)
            } else {
                Var(x)
            }
        }
        LoadLabel(label) => {
            if let Some(x) = global_int.get(&label.0) {
                Li(*x)
            } else if let Some(x) = global_float.get(&label.0) {
                FLi(*x)
            } else {
                LoadLabel(label)
            }
        }
        IntOp(kind, x, Value::Imm(y)) => {
            if let Some(xx) = env.get(&x) {
                match kind {
                    IntOpKind::Mul16 => Li((xx * y) as i32),
                    IntOpKind::Add => Li((xx + y) as i32),
                    IntOpKind::Sub => Li((xx - y) as i32),
                    IntOpKind::Shl => Li((*xx as i32) << y),
                    IntOpKind::Shr => Li((*xx as i32) >> y),
                }
            } else {
                IntOp(kind, x, Value::Imm(y))
            }
        }
        IntOp(kind, x, Value::Var(y)) => {
            if let Some(y) = env.get(&y) {
                match kind {
                    IntOpKind::Mul16 => {
                        if *y == 0 {
                            Li(0)
                        } else if *y == 1 {
                            Var(x)
                        } else {
                            IntOp(IntOpKind::Mul16, x, Value::Imm(*y))
                        }
                    }
                    kind => {
                        if *y == 0 {
                            Var(x)
                        } else {
                            IntOp(kind, x, Value::Imm(*y))
                        }
                    }
                }
            } else if let Some(xx) = env.get(&x) {
                match kind {
                    IntOpKind::Mul16 => {
                        if *xx == 0 {
                            Li(0)
                        } else if *xx == 1 {
                            Var(y)
                        } else {
                            IntOp(IntOpKind::Mul16, y, Value::Imm(*xx))
                        }
                    }
                    _ if *xx == 0 => Var(y),
                    IntOpKind::Add => IntOp(IntOpKind::Add, y, Value::Imm(*xx)),
                    kind => IntOp(kind, x, Value::Var(y)),
                }
            } else {
                IntOp(kind, x, Value::Var(y))
            }
        }
        If(IfKind::IfLE, x, Value::Var(y), e1, e2) => {
            let e1 = conv(e1, global_int, global_float, env);
            let e2 = conv(e2, global_int, global_float, env);
            if let Some(x) = env.get(&x) {
                If(IfKind::IfGE, y, Value::Imm(*x), e1, e2)
            } else if let Some(y) = env.get(&y) {
                If(IfKind::IfLE, x, Value::Imm(*y), e1, e2)
            } else {
                If(IfKind::IfLE, x, Value::Var(y), e1, e2)
            }
        }
        If(IfKind::IfEq, x, Value::Var(y), e1, e2) => {
            let e1 = conv(e1, global_int, global_float, env);
            let e2 = conv(e2, global_int, global_float, env);
            if let Some(x) = env.get(&x) {
                If(IfKind::IfEq, y, Value::Imm(*x), e1, e2)
            } else if let Some(y) = env.get(&y) {
                If(IfKind::IfEq, x, Value::Imm(*y), e1, e2)
            } else {
                If(IfKind::IfEq, x, Value::Var(y), e1, e2)
            }
        }
        If(kind, x, y, e1, e2) => {
            let e1 = conv(e1, global_int, global_float, env);
            let e2 = conv(e2, global_int, global_float, env);
            If(kind, x, map!(y), e1, e2)
        }
        Let(v, e1, e2) => {
            let e1 = conv(e1, global_int, global_float, env);
            if let Some(v) = &v {
                match &e1.item {
                    Li(x) if i16::MIN as i32 <= *x && *x <= i16::MAX as i32 => {
                        env.insert(v.clone(), *x as i16);
                    }
                    _ => (),
                };
            }

            let e2 = conv(e2, global_int, global_float, env);

            Let(v, e1, e2)
        }
        IfF(kind, x, y, e1, e2) => {
            let e1 = conv(e1, global_int, global_float, env);
            let e2 = conv(e2, global_int, global_float, env);
            IfF(kind, x, y, e1, e2)
        }
        AllocHeap(x) => AllocHeap(map!(x)),
        Lw(x, y) => Lw(x, map!(y)),
        Sw(x, y, z) => Sw(x, map!(y), z),
        Loop { vars, init, body } => {
            let init = init.into_iter().map(|x| map!(x)).collect();
            let body = conv(body, global_int, global_float, env);
            Loop { vars, init, body }
        }
        CallDir(label, args) => {
            let args = args.into_iter().map(|x| map!(x)).collect();
            CallDir(label, args)
        }
        CallCls(f, args) => {
            let args = args.into_iter().map(|x| map!(x)).collect();
            CallCls(f, args)
        }
        item => item,
    };

    e
}

pub fn simm(mut p: Program) -> Program {
    let mut global_int = Map::default();
    let mut global_float = Map::default();
    let mut env = Map::default();
    for (g, data) in &p.globals {
        match data {
            GlobalData::GInt16(i) => {
                global_int.insert(g.clone(), *i as i32);
                env.insert(g.clone(), *i);
            }
            GlobalData::GInt32(i) => {
                global_int.insert(g.clone(), *i);
            }
            GlobalData::GFloat(x) => {
                global_float.insert(g.clone(), *x);
            }
            _ => (),
        };
    }

    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(buf, &global_int, &global_float, &mut env);
    }

    p.main = conv(p.main, &global_int, &global_float, &mut env);

    p
}
