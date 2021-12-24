use util::Spanned;

use ast::knormal::*;

// 実際の変換
fn rotate(decl: Decl, cont: Box<Expr>, e: Box<Expr>) -> Box<Expr> {
    use LetKind::*;
    let loc = e.loc;
    let kind = match e.item {
        ExprKind::Let(l) => {
            match l {
                Let(d, e1, e2) => Let(d, e1, rotate(decl, cont, e2)),
                LetRec(fd, e2) => LetRec(fd, rotate(decl, cont, e2)),
                LetTuple(ds, x, e2) => LetTuple(ds, x, rotate(decl, cont, e2)),
            }
        },
        _ => Let(decl, e, conv(cont))
    };

    Box::new(Spanned::new(ExprKind::Let(kind), loc))
}

// 変換の呼び出し
fn conv(mut e: Box<Expr>) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        If(kind, x, y, e1, e2) => If(kind, x, y, conv(e1), conv(e2)),
        Let(l) => {
            use LetKind::*;
            let kind = match l {
                Let(decl, e1, e2) => return rotate(decl, e2, conv(e1)),
                LetRec(Fundef { fvar, args, body } , e2) => LetRec(Fundef { fvar, args, body: conv(body) }, conv(e2)),
                LetTuple(ds, x, e2) => LetTuple(ds, x, conv(e2))
            };

            ExprKind::Let(kind)
        },
        Loop { vars, init, body } => Loop { vars, init, body: conv(body) },
        _ => return e
    };
    
    e
}

// `let x = (letkind y = e1 in e2) in ...`
// を
// ```
// letkind y = e1 in
// let x = e2 in
// ...
// ```
// に変換する
pub fn flatten_let(e: Expr) -> Expr {
    *conv(Box::new(e))
}