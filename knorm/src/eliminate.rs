use util::Id;

use ast::knormal::*;

type Set = util::Set<Id>;

fn has_effect(e: &Expr) -> bool {
    use ExprKind::*;
    match &e.item {
        If(_, _, _, e1, e2) => has_effect(&e1) || has_effect(&e2),
        Let(_, e1, e2) => has_effect(&e1) || has_effect(&e2),
        LetRec(_, e2) => has_effect(&e2),
        Loop { body, .. } => has_effect(&body),
        App(..) | ExtApp(..) | ArrayPut(..) | AsmE(..) => true,
        _ => false,
    }
}

// 変換の呼び出し
fn conv(mut e: Box<Expr>, used: &mut Set, tyenv: &mut TyMap) -> Box<Expr> {
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
            let e1 = conv(e1, used, tyenv);
            let e2 = conv(e2, used, tyenv);
            used.insert(x.clone());
            used.insert(y.clone());
            If(kind, x, y, e1, e2)
        }
        Let(d, e1, e2) => {
            let e2 = conv(e2, used, tyenv);
            if !used.contains(&d.name) && !has_effect(&e1) {
                log::debug!("eliminating variable `{}`.", d.name);
                tyenv.remove(&d.name);
                return e2;
            }

            Let(d, conv(e1, used, tyenv), e2)
        }
        LetRec(Fundef { fvar, args, body }, e2) => {
            let e2 = conv(e2, used, tyenv);
            if !used.contains(&fvar.name) {
                log::info!("eliminating function `{}`.", fvar.name);
                tyenv.remove(&fvar.name);
                return e2;
            }

            let body = conv(body, used, tyenv);
            LetRec(Fundef { fvar, args, body }, e2)
        }
        Tuple(xs) => {
            for x in &xs {
                used.insert(x.clone());
            }
            Tuple(xs)
        }
        App(f, args) => {
            used.insert(f.clone());
            for x in &args {
                used.insert(x.clone());
            }
            App(f, args)
        }
        ExtApp(f, args) => {
            for x in &args {
                used.insert(x.clone());
            }
            ExtApp(f, args)
        }
        CreateArray(x, y) => {
            used.insert(x.clone());
            used.insert(y.clone());
            CreateArray(x, y)
        }
        ExtArray(x) => {
            used.insert(x.clone());
            ExtArray(x)
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
            let body = conv(body, used, tyenv);
            for x in &init {
                used.insert(x.clone());
            }
            Loop { vars, init, body }
        }
        Continue(xs) => {
            for (_, x) in &xs {
                used.insert(x.clone());
            }
            Continue(xs)
        }
        Asm(inst, args) => {
            for x in &args {
                used.insert(x.clone());
            }
            Asm(inst, args)
        }
        AsmE(inst, args) => {
            for x in &args {
                used.insert(x.clone());
            }
            AsmE(inst, args)
        }
        Const(_) => e.item,
    };

    e
}

// 不要定義削除を行う
pub fn eliminate(e: Expr, tyenv: &mut TyMap) -> Expr {
    *conv(Box::new(e), &mut Set::default(), tyenv)
}
