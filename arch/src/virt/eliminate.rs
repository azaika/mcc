use super::program::*;
use ast::closure::Label;
use util::{Id, Set};

fn can_remove(e: &Expr) -> bool {
    use ExprKind::*;
    match &e.item {
        Nop | Var(_) | Li(_) | FLi(_) | GetLabel(_) | LoadLabel(_) | UnOp(..) | IntOp(..)
        | FloatOp(..) | AllocHeap(_) | Lw(..) => true,
        _ => false,
    }
}

fn is_global_load(e: &Expr) -> bool {
    match &e.item {
        ExprKind::GetLabel(..) | ExprKind::LoadLabel(..) => true,
        _ => false
    }
}

fn conv(
    mut e: Box<Expr>,
    tyenv: &mut TyMap,
    used: &mut Set<Id>,
    global_used: &mut Set<Label>,
) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        Let(None, e1, e2) => {
            let e2 = conv(e2, tyenv, used, global_used);
            let e1 = conv(e1, tyenv, used, global_used);
            Let(None, e1, e2)
        }
        Let(Some(v), e1, e2) => {
            let e2 = conv(e2, tyenv, used, global_used);
            if !used.contains(&v) {
                if !is_global_load(&e1) {
                    tyenv.remove(&v);
                }
                if can_remove(&e1) {
                    return e2;
                } else {
                    let e1 = conv(e1, tyenv, used, global_used);
                    Let(None, e1, e2)
                }
            } else {
                let e1 = conv(e1, tyenv, used, global_used);
                Let(Some(v), e1, e2)
            }
        }
        Var(x) => {
            used.insert(x.clone());
            Var(x)
        }
        GetLabel(label) => {
            global_used.insert(label.clone());
            GetLabel(label)
        }
        LoadLabel(label) => {
            global_used.insert(label.clone());
            LoadLabel(label)
        }
        UnOp(kind, x) => {
            used.insert(x.clone());
            UnOp(kind, x)
        }
        IntOp(kind, x, y) => {
            used.insert(x.clone());
            if let Value::Var(y) = &y {
                used.insert(y.clone());
            }
            IntOp(kind, x, y)
        }
        FloatOp(kind, x, y) => {
            used.insert(x.clone());
            used.insert(y.clone());
            FloatOp(kind, x, y)
        }
        If(kind, x, y, e1, e2) => {
            let e1 = conv(e1, tyenv, used, global_used);
            let e2 = conv(e2, tyenv, used, global_used);
            used.insert(x.clone());
            if let Value::Var(y) = &y {
                used.insert(y.clone());
            }

            If(kind, x, y, e1, e2)
        }
        IfF(kind, x, y, e1, e2) => {
            let e1 = conv(e1, tyenv, used, global_used);
            let e2 = conv(e2, tyenv, used, global_used);
            used.insert(x.clone());
            used.insert(y.clone());

            IfF(kind, x, y, e1, e2)
        }
        CallDir(label, xs) => {
            global_used.insert(label.clone());
            for x in &xs {
                used.insert(x.clone());
            }
            CallDir(label, xs)
        }
        CallCls(f, xs) => {
            used.insert(f.clone());
            for x in &xs {
                used.insert(x.clone());
            }
            CallCls(f, xs)
        }
        AllocHeap(Value::Var(x)) => {
            used.insert(x.clone());
            AllocHeap(Value::Var(x))
        }
        Lw(x, y) => {
            used.insert(x.clone());
            if let Value::Var(y) = &y {
                used.insert(y.clone());
            }
            Lw(x, y)
        }
        Sw(x, y, z) => {
            used.insert(x.clone());
            used.insert(z.clone());
            if let Value::Var(y) = &y {
                used.insert(y.clone());
            }
            Sw(x, y, z)
        }
        Loop { vars, init, body } => {
            let body = conv(body, tyenv, used, global_used);
            for x in &init {
                if let Value::Var(x) = x {
                    used.insert(x.clone());
                }
            }

            Loop { vars, init, body }
        }
        Continue(ps) => {
            for (_, x) in &ps {
                used.insert(x.clone());
            }
            Continue(ps)
        }
        Out(x) => {
            used.insert(x.clone());
            Out(x)
        }
        _ => e.item,
    };

    e
}

pub fn eliminate(mut p: Program) -> Program {
    let mut global_used: Set<Label> = Set::default();
    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(buf, &mut p.tyenv, &mut Set::default(), &mut global_used);
    }

    p.main = conv(p.main, &mut p.tyenv, &mut Set::default(), &mut global_used);

    let mut new_globals = vec![];
    for (g, data) in p.globals {
        if global_used.contains(&Label(g.clone())) {
            new_globals.push((g, data));
        }
        else {
            p.tyenv.remove(&g);
        }
    }
    p.globals = new_globals;

    p
}
