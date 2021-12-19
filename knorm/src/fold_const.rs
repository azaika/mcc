use util::Spanned;

use ast::knormal::*;

// `let x = (letkind y = e1 in e2) in ...`
// を
// ```
// letkind y = e1 in
// let x = e2 in
// ...
// ```
// に変換する
pub fn fold_const(e: Expr) -> Expr {
    todo!()
}