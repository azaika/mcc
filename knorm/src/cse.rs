use std::hash::Hash;

use ast::knormal::*;
use util::{Id, Spanned};

// for comparison considering commutativity
// ignoring float constant NaN, which do not emerge in constant context
// so this provides `Eq` trait
#[derive(Debug, Clone)]
struct ExprInVal(Box<Expr>);

fn eq_in_val(e: &Expr, other: &Expr) -> bool {
    use ExprKind::*;
    match (&e.item, &other.item) {
        (BinOp(op1, x1, x2), BinOp(op2, y1, y2)) if op1 == op2 => {
            use BinOpKind::*;
            match op1 {
                Add | FAdd | Mul | FMul => (x1, x2) == (y1, y2) || (x2, x1) == (y1, y2),
                Sub | FSub | Div | FDiv => (x1, x2) == (y1, y2),
            }
        }
        (If(k1, x1, x2, e1, e2), If(k2, y1, y2, e3, e4)) if k1 == k2 => {
            let comp_eq = match k1 {
                IfKind::IfEq => (x1, x2) == (y1, y2) || (x1, x2) == (y2, y1),
                IfKind::IfLE => (x1, x2) == (y1, y2),
            };
            comp_eq && eq_in_val(e1, e3) && eq_in_val(e2, e4)
        }
        (Let(l1), Let(l2)) => {
            match (l1, l2) {
                (LetKind::Let(d1, e1, e2), LetKind::Let(d2, e3, e4)) => {
                    if d1.t == d2.t && eq_in_val(e1, e3) {
                        if d1.name == d2.name {
                            eq_in_val(e2, e4)
                        } else {
                            let mut m = util::Map::default();
                            m.insert(d2.name.clone(), d1.name.clone());
                            rename(e4.clone(), &m);
                            eq_in_val(e2, &e4)
                        }
                    } else {
                        false
                    }
                }
                _ => l1 == l2, // for simplicity
            }
        }
        (Loop { .. }, Loop { .. }) => e.item == other.item, // for simplicity
        _ => e.item == other.item,
    }
}

impl PartialEq for ExprInVal {
    fn eq(&self, other: &Self) -> bool {
        eq_in_val(&self.0, &other.0)
    }
}
impl Eq for ExprInVal {}

fn hash_impl<H: std::hash::Hasher>(e: &Expr, state: &mut H, num_let: usize) {
    core::mem::discriminant(&e.item).hash(state);
    use ExprKind::*;
    match &e.item {
        Const(c) => c.hash(state),
        Var(x) | ExtArray(x) | Load(x) => x.hash(state),
        UnOp(op, x) => {
            op.hash(state);
            x.hash(state)
        }
        BinOp(op, x, y) => {
            op.hash(state);
            use BinOpKind::*;
            let (x, y) = match op {
                Add | FAdd | Mul | FMul if x > y => (y, x),
                _ => (x, y),
            };

            x.hash(state);
            y.hash(state)
        }
        If(kind, x, y, e1, e2) => {
            kind.hash(state);
            let (x, y) = if x < y { (x, y) } else { (y, x) };
            x.hash(state);
            y.hash(state);
            hash_impl(&e1, state, num_let);
            hash_impl(&e2, state, num_let)
        }
        Let(l) => match l {
            LetKind::Let(d, e1, e2) => {
                const HASHLING_LIMIT: usize = 5;
                if num_let < HASHLING_LIMIT {
                    hash_impl(&e1, state, num_let);
                    let new_name = format!("V{}{}", d.t.short(), num_let);
                    new_name.hash(state);
                    let mut m = util::Map::default();
                    m.insert(d.name.clone(), new_name);
                    let e2 = rename(Box::new(e2.as_ref().clone()), &m);
                    hash_impl(&e2, state, num_let + 1)
                }
            }
            LetKind::LetRec(Fundef { fvar, args, body }, e2) => {
                fvar.hash(state);
                args.hash(state);
                hash_impl(&body, state, num_let);
                hash_impl(&e2, state, num_let);
            }
            LetKind::LetTuple(ds, x, e2) => {
                ds.hash(state);
                x.hash(state);
                hash_impl(&e2, state, num_let);
            }
        },
        Tuple(xs) => xs.hash(state),
        App(f, args) | ExtApp(f, args) => {
            f.hash(state);
            args.hash(state)
        }
        CreateArray(x, y) | Get(x, y) | Assign(x, y) => {
            x.hash(state);
            y.hash(state)
        }
        Put(x, y, z) => {
            x.hash(state);
            y.hash(state);
            z.hash(state)
        }
        Loop { .. } | Continue(_) => {
            // do nothing because loop is not target of CSE
        },
    }
}

impl Hash for ExprInVal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        hash_impl(&self.0, state, 0);
    }
}

type Set = util::Set<Id>;
type Map = util::Map<ExprInVal, Id>;

fn is_impure(e: &Expr, effects: &mut Set) -> bool {
    use ExprKind::*;
    match &e.item {
        If(_, _, _, e1, e2) => is_impure(e1, effects) || is_impure(e2, effects),
        Let(l) => match l {
            LetKind::Let(_, e1, e2) => is_impure(e1, effects) || is_impure(e2, effects),
            LetKind::LetRec(fundef, e2) => {
                if is_impure(&fundef.body, effects) {
                    effects.insert(fundef.fvar.name.clone());
                }

                let r = is_impure(e2, effects);

                effects.remove(&fundef.fvar.name);

                r
            }
            LetKind::LetTuple(_, _, e2) => is_impure(e2, effects),
        },
        App(f, _) => effects.contains(f),
        ExtApp(_, _) | CreateArray(_, _) | ExtArray(_) | Put(_, _, _) | Get(_, _) | Loop { .. } | Assign(_, _) | Load(_) => {
            true
        }
        _ => false,
    }
}

fn conv(mut e: Box<Expr>, effects: &mut Set, saved: &mut Map) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        If(kind, x, y, e1, e2) => If(
            kind,
            x,
            y,
            conv(e1, effects, saved),
            conv(e2, effects, saved),
        ),
        Let(l) => {
            let kind = match l {
                LetKind::Let(d, e1, e2) => {
                    let key = ExprInVal(e1);
                    if let Some(x) = saved.get(&key) {
                        log::info!("found common sub-expressions `{}` and `{}`.", d.name, x);
                        let mut m = util::Map::default();
                        m.insert(d.name.clone(), x.clone());
                        LetKind::Let(
                            d,
                            Box::new(Spanned::new(Var(x.clone()), e.loc)),
                            conv(e2, effects, saved),
                        )
                    } else {
                        let e1 = key.0;
                        if !is_impure(&e1, effects) {
                            let key = ExprInVal(e1.clone());
                            saved.insert(key.clone(), d.name.clone());
                            let r =
                                LetKind::Let(d, conv(e1, effects, saved), conv(e2, effects, saved));
                            saved.remove(&key);
                            r
                        } else {
                            LetKind::Let(d, conv(e1, effects, saved), conv(e2, effects, saved))
                        }
                    }
                }
                LetKind::LetRec(Fundef { fvar, args, body }, e2) => {
                    // この宣言が副作用を持つか調べる
                    if is_impure(&body, effects) {
                        effects.insert(fvar.name.clone());
                    }

                    let body = conv(body, effects, saved);
                    let e2 = conv(e2, effects, saved);
                    LetKind::LetRec(Fundef { fvar, args, body }, e2)
                }
                LetKind::LetTuple(ds, x, e2) => LetKind::LetTuple(ds, x, conv(e2, effects, saved)),
            };

            Let(kind)
        }
        Loop { vars, loop_vars, init, body } => Loop {
            vars,
            loop_vars,
            init,
            body: conv(body, effects, saved),
        },
        _ => e.item,
    };

    e
}

// common sub-expression elimination
// assume `e` is alpha formed
pub fn cse(e: Expr) -> Expr {
    *conv(Box::new(e), &mut Set::default(), &mut Map::default())
}
