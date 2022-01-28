use util::Id;

use ast::closure::*;

type Set = util::Set<Id>;

fn has_effect(e: &Expr) -> bool {
    use ExprKind::*;
    match &e.item {
        If(_, _, _, e1, e2) | Let(_, e1, e2) => has_effect(&e1) || has_effect(&e2),
        Loop { body, .. } | DoAll { body, .. } => has_effect(&body),
        CallDir(..) | CallCls(..) | ArrayPut(..) | Assign(..) | Asm(..) => true,
        _ => false,
    }
}

// 変換の呼び出し
fn conv(mut e: Box<Expr>, globals: &Vec<Id>, used: &mut Set, tyenv: &mut TyMap) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        Var(x) => {
            used.insert(x.clone());
            Var(x)
        }
        UnOp(op, x) => {
            used.insert(x.clone());
            UnOp(op, x)
        }
        BinOp(op, x, y) => {
            used.insert(x.clone());
            used.insert(y.clone());
            BinOp(op, x, y)
        }
        If(kind, x, y, e1, e2) => {
            let e1 = conv(e1, globals, used, tyenv);
            let e2 = conv(e2, globals, used, tyenv);
            used.insert(x.clone());
            used.insert(y.clone());
            If(kind, x, y, e1, e2)
        }
        Let(d, e1, e2) => {
            let e2 = conv(e2, globals, used, tyenv);
            if !used.contains(&d) && !has_effect(&e1) && !globals.contains(&d) {
                log::debug!("eliminating variable `{}`.", d);
                tyenv.remove(&d);
                return e2;
            }

            Let(d, conv(e1, globals, used, tyenv), e2)
        }
        Tuple(xs) => {
            for x in &xs {
                used.insert(x.clone());
            }
            Tuple(xs)
        }
        CallCls(f, args) => {
            used.insert(f.clone());
            for x in &args {
                used.insert(x.clone());
            }
            CallCls(f, args)
        }
        CallDir(label, args) => {
            for x in &args {
                used.insert(x.clone());
            }
            CallDir(label, args)
        }
        AllocArray(x, y, z) => {
            used.insert(x.clone());
            if let Some(z) = &z {
                used.insert(z.clone());
            }
            AllocArray(x, y, z)
        }
        ArrayGet(x, y) => {
            used.insert(x.clone());
            used.insert(y.clone());
            ArrayGet(x, y)
        }
        ArrayPut(x, y, z) => {
            used.insert(x.clone());
            used.insert(y.clone());
            used.insert(z.clone());
            ArrayPut(x, y, z)
        }
        TupleGet(x, idx) => {
            used.insert(x.clone());
            TupleGet(x, idx)
        }
        Loop { vars, init, body } => {
            let body = conv(body, globals, used, tyenv);
            for x in &init {
                used.insert(x.clone());
            }
            Loop { vars, init, body }
        }
        DoAll {
            idx,
            range,
            delta,
            body,
        } => {
            let body = conv(body, globals, used, tyenv);
            used.insert(idx.clone());
            used.insert(range.0.clone());
            used.insert(range.1.clone());

            DoAll {
                idx,
                range,
                delta,
                body,
            }
        }
        Continue(xs) => {
            for (_, x) in &xs {
                used.insert(x.clone());
            }
            Continue(xs)
        }
        MakeCls(label, xs) => {
            for x in &xs {
                used.insert(x.clone());
            }
            MakeCls(label, xs)
        }
        Assign(label, x) => {
            used.insert(x.clone());
            Assign(label, x)
        }
        Asm(inst, args) => {
            for x in &args {
                used.insert(x.clone());
            }
            Asm(inst, args)
        }
        Const(_) | Load(_) | ExtArray(_) => e.item,
    };

    e
}

// 不要定義削除を行う
pub fn eliminate_var(mut p: Program) -> Program {
    let mut used = Set::default();
    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(buf, &p.globals, &mut used, &mut p.tyenv);
    }
    p.main = conv(p.main, &p.globals, &mut used, &mut p.tyenv);

    {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(&mut p.global_init, &mut buf);
        p.global_init = conv(buf, &p.globals, &mut used, &mut p.tyenv);
    }

    p
}
