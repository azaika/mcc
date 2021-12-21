use ast::knormal::*;
use util::{Id, Spanned};

// for comparison considering commutativity
// ignoring float constant NaN, which is not emerge in constant context
// so this provides `Eq` trait
#[derive(Debug, Clone, Hash)]
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
        },
        (If(k1, x1, x2, e1, e2), If(k2, y1, y2, e3, e4)) if k1 == k2 => {
            let comp_eq = match k1 {
                IfKind::IfEq => (x1, x2) == (y1, y2) || (x1, x2) == (y2, y1),
                IfKind::IfLE => (x1, x2) == (y1, y2),
            };
            comp_eq && eq_in_val(e1, e3) && eq_in_val(e2, e4)
        },
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
                    }
                    else {
                        false
                    }
                },
                _ => l1 == l2 // for simplicity
            }
        },
        _ => e.item == other.item
    }
}

impl PartialEq for ExprInVal {
    fn eq(&self, other: &Self) -> bool {
        eq_in_val(&self.0, &other.0)
    }
}
impl Eq for ExprInVal {}

type Set = util::Set<Id>;
type Map = util::Map<ExprInVal, Id>;

fn has_side_effect(e: &Expr, effects: &mut Set) -> bool {
    use ExprKind::*;
    match &e.item {
        If(_, _, _, e1, e2) => has_side_effect(e1, effects) || has_side_effect(e2, effects),
        Let(l) => {
            match l {
                LetKind::Let(_, e1, e2) => has_side_effect(e1, effects) || has_side_effect(e2, effects),
                LetKind::LetRec(fundef, e2) => {
                    if has_side_effect(&fundef.body, effects) {
                        effects.insert(fundef.fvar.name.clone());
                    }

                    let r = has_side_effect(e2, effects);

                    effects.remove(&fundef.fvar.name);

                    r
                },
                LetKind::LetTuple(_, _, e2) => has_side_effect(e2, effects),
            }
        },
        App(f, _) => effects.contains(f),
        ExtApp(_, _) | CreateArray(_, _) | ExtArray(_) | Put(_, _, _) => true,
        _ => false
    }
}

fn conv(mut e: Box<Expr>, effects: &mut Set, saved: &mut Map) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        If(kind, x, y, e1, e2) => If(kind, x, y, conv(e1, effects, saved), conv(e2, effects, saved)),
        Let(l) => {
            let kind = match l {
                LetKind::Let(d, e1, e2) => {
                    let key = ExprInVal(e1);
                    if let Some(x) = saved.get(&key) {
                        // saved expression found
                        log::info!("eliminating common sub-expressions `{}` to `{}`.", d.name, x);
                        let mut m = util::Map::default();
                        m.insert(d.name.clone(), x.clone());
                        LetKind::Let(d, Box::new(Spanned::new(Var(x.clone()), e.loc)), conv(e2, effects, saved))
                    }
                    else {
                        let e1 = key.0;
                        if !has_side_effect(&e1, effects) {
                            saved.insert(ExprInVal(e1.clone()), d.name.clone());
                        }
                        LetKind::Let(d, conv(e1, effects, saved), conv(e2, effects, saved))
                    }
                },
                LetKind::LetRec(Fundef { fvar, args, body }, e2) => {
                    // この宣言が副作用を持つか調べる
                    if has_side_effect(&body, effects) {
                        effects.insert(fvar.name.clone());
                    }

                    let body = conv(body, effects, saved);
                    let e2 = conv(e2, effects, saved);
                    LetKind::LetRec(Fundef { fvar, args, body }, e2)
                },
                LetKind::LetTuple(ds, x, e2) => LetKind::LetTuple(ds, x, conv(e2, effects, saved)),
            };

            Let(kind)
        },
        _ => e.item
    };

    e
}

// common sub-expression elimination
// assume `e` is alpha formed
pub fn cse(e: Expr) -> Expr {
    *conv(Box::new(e), &mut Set::default(), &mut Map::default())
}