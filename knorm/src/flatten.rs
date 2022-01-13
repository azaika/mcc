use ast::knormal::*;
use util::ToSpanned;

// 実際の変換
fn rotate(decl: Decl, cont: Box<Expr>, span: util::Span, mut e: Box<Expr>) -> Box<Expr> {
    e.item = match e.item {
        ExprKind::Let(d, e1, e2) => ExprKind::Let(d, e1, rotate(decl, cont, span, e2)),
        ExprKind::LetRec(fd, e2) => ExprKind::LetRec(fd, rotate(decl, cont, span, e2)),
        item => {
            return Box::new(
                ExprKind::Let(decl, Box::new(item.with_span(e.loc)), cont).with_span(span),
            )
        }
    };

    e
}

// 変換の呼び出し
fn conv(mut e: Box<Expr>) -> Box<Expr> {
    use ExprKind::*;
    e.item = match e.item {
        If(kind, x, y, e1, e2) => If(kind, x, y, conv(e1), conv(e2)),
        Let(decl, e1, e2) => return rotate(decl, conv(e2), e.loc, conv(e1)),
        LetRec(Fundef { fvar, args, body }, e2) => LetRec(
            Fundef {
                fvar,
                args,
                body: conv(body),
            },
            conv(e2),
        ),
        Loop { vars, init, body } => Loop {
            vars,
            init,
            body: conv(body),
        },
        _ => return e,
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
