use util::Id;

use ast::knormal::*;

type Set = util::Set<Id>;

fn has_effect(e: &Expr) -> bool {
    use ExprKind::*;
    match &e.item {
        If(_, _, _, e1, e2) => has_effect(&e1) || has_effect(&e2),
        Let(l) => {
            match l {
                LetKind::Let(_, e1, e2) => has_effect(&e1) || has_effect(&e2),
                LetKind::LetRec(_, e2) => has_effect(&e2),
            }
        },
        Loop { body, .. } => has_effect(&body),
        App(_, _) | ExtApp(_, _) | ArrayPut(_, _, _) => true,
        _ => false,
    }
}

// 変換の呼び出し
fn conv(mut e: Box<Expr>, used: &mut Set) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        Var(x) => {
            used.insert(x.clone());
            Var(x)
        },
        UnOp(op, x) => {
            used.insert(x.clone());
            UnOp(op, x)
        },
        BinOp(op, x, y) => {
            used.insert(x.clone());
            used.insert(y.clone());
            BinOp(op, x, y)
        },
        If(kind, x, y, e1, e2) => {
            let e1 = conv(e1, used);
            let e2 = conv(e2, used);
            used.insert(x.clone());
            used.insert(y.clone());
            If(kind, x, y, e1, e2)
        },
        Let(l) => {
            let kind = match l {
                LetKind::Let(d, e1, e2) => {
                    let e2 = conv(e2, used);
                    if !used.contains(&d.name) && !has_effect(&e1) {
                        log::debug!("eliminating variable `{}`.", d.name);
                        return e2;
                    }

                    LetKind::Let(d, conv(e1, used), e2)
                },
                LetKind::LetRec(Fundef { fvar, args, body }, e2) => {
                    let e2 = conv(e2, used);
                    if !used.contains(&fvar.name) {
                        log::info!("eliminating function `{}`.", fvar.name);
                        return e2;
                    }
                    
                    let body = conv(body, used);
                    LetKind::LetRec(Fundef{ fvar, args, body }, e2)
                },
            };

            Let(kind)
        },
        Tuple(xs) => {
            for x in &xs {
                used.insert(x.clone());
            }
            Tuple(xs)
        },
        App(f, args) => {
            used.insert(f.clone());
            for x in &args {
                used.insert(x.clone());
            };
            App(f, args)
        },
        ExtApp(f, args) => {
            for x in &args {
                used.insert(x.clone());
            };
            ExtApp(f, args)
        },
        CreateArray(x, y) => {
            used.insert(x.clone());
            used.insert(y.clone());
            CreateArray(x, y)
        },
        ExtArray(x) => {
            used.insert(x.clone());
            ExtArray(x)
        },
        ArrayGet(x, y) => {
            used.insert(x.clone());
            used.insert(y.clone());
            ArrayGet(x, y)
        },
        ArrayPut(x, y, z) => {
            used.insert(x.clone());
            used.insert(y.clone());
            used.insert(z.clone());
            ArrayPut(x, y, z)
        },
        Loop { vars, loop_vars, init, body } => {
            let body = conv(body, used);
            for x in &init {
                used.insert(x.clone());
            };
            Loop { vars, loop_vars, init, body }
        },
        Continue(xs) => {
            for (_, x) in &xs {
                used.insert(x.clone());
            };
            Continue(xs)
        },
        _ => e.item
    };
    
    e
}

// 不要定義削除を行う
pub fn eliminate(e: Expr) -> Expr {
    *conv(Box::new(e), &mut Set::default())
}