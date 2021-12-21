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
                LetKind::LetRec(_, e2) | LetKind::LetTuple(_, _, e2) => has_effect(&e2),
            }
        },
        App(_, _) | ExtApp(_, _) | CreateArray(_, _) | Put(_, _, _) => true,
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
                        log::info!("eliminating variable `{}`.", d.name);
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
                LetKind::LetTuple(ds, x, e2) => {
                    let e2 = conv(e2, used);
                    let necessary = ds.iter().any(|d| used.contains(&d.name));
                    if !necessary {
                        log::info!("eliminating variables {:?}.", ds.iter().map(|d| &d.name).collect::<Vec<_>>());
                        return e2;
                    }

                    used.insert(x.clone());
                    LetKind::LetTuple(ds, x, e2)
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
        Get(x, y) => {
            used.insert(x.clone());
            used.insert(y.clone());
            Get(x, y)
        },
        Put(x, y, z) => {
            used.insert(x.clone());
            used.insert(y.clone());
            used.insert(z.clone());
            Put(x, y, z)
        },
        _ => e.item
    };
    
    e
}

pub fn eliminate(e: Expr) -> Expr {
    *conv(Box::new(e), &mut Set::default())
}