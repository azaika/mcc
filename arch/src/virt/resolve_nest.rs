use super::program::*;
use util::{Id, ToSpanned};

// 実際の変換
fn rotate(v: Option<Id>, cont: Box<Expr>, span: util::Span, mut e: Box<Expr>) -> Box<Expr> {
    e.item = match e.item {
        ExprKind::Let(d, e1, e2) => ExprKind::Let(d, e1, rotate(v, cont, span, e2)),
        item => {
            return Box::new(
                ExprKind::Let(v, Box::new(item.with_span(e.loc)), conv(cont)).with_span(span),
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
        Let(decl, e1, e2) => return rotate(decl, e2, e.loc, conv(e1)),
        Loop { vars, init, body } => Loop {
            vars,
            init,
            body: conv(body),
        },
        _ => e.item,
    };

    e
}

pub fn resolve_nest(mut p: Program) -> Program {
    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(buf);
    }

    p.main = conv(p.main);

    p
}