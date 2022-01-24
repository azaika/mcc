use ast::closure::*;
use util::{Id, Map, ToSpanned};

use crate::alias::AliasMap;
use crate::alias::{self, may_overlap};
use crate::common::ConstMap;

type Saved = Map<alias::Alias, Id>;
type SavedUnknown = Map<Ty, Map<(Id, alias::Index), Id>>;

fn conv_array_get(
    arr: Id,
    idx: Id,
    v: Option<Id>,
    consts: &ConstMap,
    tyenv: &TyMap,
    aliases: &AliasMap,
    saved: &mut Saved,
    unknown_saved: &mut SavedUnknown,
) -> ExprKind {
    use ExprKind::*;

    let index = if let Some(ConstKind::CInt(s)) = consts.get(&idx) {
        alias::Index::Int(s.abs() as usize)
    } else {
        alias::Index::Var(idx.clone())
    };

    if let Some(mut a) = aliases.get(&arr).cloned() {
        a.belongs.push(index);
        if let Some(x) = saved.get(&a) {
            Var(x.clone())
        } else {
            v.map(|v| saved.insert(a.clone(), v.clone()));
            ArrayGet(arr, idx)
        }
    } else {
        let t = tyenv.get(&arr).unwrap().clone().decay();
        let arr_saved = unknown_saved.entry(t).or_default();
        let key = (arr.clone(), index);
        if let Some(x) = arr_saved.get(&key).cloned() {
            Var(x.clone())
        } else {
            v.map(|v| arr_saved.insert(key, v.clone()));
            ArrayGet(arr, idx)
        }
    }
}

fn collect_puts(e: &Expr, consts: &ConstMap, written: &mut util::Set<(Id, alias::Index)>) {
    use ExprKind::*;
    match &e.item {
        ArrayPut(arr, idx, _) => {
            let index = if let Some(ConstKind::CInt(idx)) = consts.get(idx) {
                alias::Index::Int(idx.abs() as usize)
            } else {
                alias::Index::Var(idx.clone())
            };

            written.insert((arr.clone(), index));
        }
        e => e.map_ref(|e| collect_puts(e, consts, written)),
    }
}

fn conv_tuple_get(
    tup: Id,
    idx: usize,
    v: Option<Id>,
    aliases: &AliasMap,
    saved: &mut Saved,
) -> ExprKind {
    use ExprKind::*;
    let index = alias::Index::Int(idx);
    if let Some(mut a) = aliases.get(&tup).cloned() {
        a.belongs.push(index);
        if let Some(x) = saved.get(&a) {
            Var(x.clone())
        } else {
            v.map(|v| saved.insert(a.clone(), v.clone()));
            TupleGet(tup, idx)
        }
    } else {
        TupleGet(tup, idx)
    }
}

// ループに入る前に保存しているとマズそうなものを削除
fn cleanse_cache(
    body: &Expr,
    consts: &ConstMap,
    tyenv: &TyMap,
    aliases: &AliasMap,
    saved: &mut Saved,
    unknown_saved: &mut SavedUnknown,
) {
    let mut written = util::Set::default();
    collect_puts(&body, consts, &mut written);
    let written = written;

    for (arr, index) in written.into_iter() {
        let t = tyenv.get(&arr).unwrap().clone().decay();
        if let Some(mut a) = aliases.get(&arr).cloned() {
            a.belongs.push(index);
            // エイリアスのある変数値の書き込み
            if let Some(arr_saved) = unknown_saved.get_mut(&t) {
                arr_saved.clear();
            }
            saved.retain(|a2, _| !may_overlap(&a, a2));
        } else {
            // エイリアスが無い変数への書き込み
            if let Some(arr_saved) = unknown_saved.get_mut(&t) {
                arr_saved.clear();
            }
            saved.retain(|_, arr| tyenv.get(arr).unwrap().clone().decay() != t);
        }
    }
}

fn conv(
    mut e: Box<Expr>,
    consts: &ConstMap,
    tyenv: &TyMap,
    aliases: &AliasMap,
    saved: &mut Saved,
    unknown_saved: &mut SavedUnknown,
) -> Box<Expr> {
    use ExprKind::*;

    e.item = match e.item {
        If(kind, x, y, e1, e2) => {
            let mut s1 = saved.clone();
            let mut us1 = unknown_saved.clone();
            let mut s2 = saved.clone();
            let mut us2 = unknown_saved.clone();

            let e1 = conv(e1, consts, tyenv, aliases, &mut s1, &mut us1);
            let e2 = conv(e2, consts, tyenv, aliases, &mut s2, &mut us2);

            // if の両方の分岐で生き残っているキャッシュのみ収集
            *saved = s1
                .drain_filter(|a, x| {
                    if let Some(y) = s2.get(a) {
                        x == y
                    } else {
                        false
                    }
                })
                .collect();

            *unknown_saved = us1
                .drain_filter(|t, m1| {
                    if let Some(m2) = us2.get(t) {
                        *m1 = m1.drain_filter(|key, _| m2.contains_key(key)).collect();
                        true
                    } else {
                        false
                    }
                })
                .collect();

            If(kind, x, y, e1, e2)
        }
        Let(v, mut e1, e2) => {
            e1.item = match e1.item {
                ArrayGet(arr, idx) => conv_array_get(
                    arr,
                    idx,
                    Some(v.clone()),
                    consts,
                    tyenv,
                    aliases,
                    saved,
                    unknown_saved,
                ),
                _ => {
                    conv(
                        Box::new(e1.item.with_span(e1.loc)),
                        consts,
                        tyenv,
                        aliases,
                        saved,
                        unknown_saved,
                    )
                    .item
                }
            };

            let e2 = conv(e2, consts, tyenv, aliases, saved, unknown_saved);
            Let(v, e1, e2)
        }
        CallDir(label, args) => {
            saved.clear();
            unknown_saved.clear();
            CallDir(label, args)
        }
        CallCls(f, args) => {
            saved.clear();
            unknown_saved.clear();
            CallCls(f, args)
        }
        ArrayGet(arr, idx) => {
            conv_array_get(arr, idx, None, consts, tyenv, aliases, saved, unknown_saved)
        }
        ArrayPut(arr, idx, x) if aliases.contains_key(&arr) => {
            let mut a = aliases.get(&arr).unwrap().clone();
            let index = if let Some(ConstKind::CInt(s)) = consts.get(&idx) {
                alias::Index::Int(s.abs() as usize)
            } else {
                alias::Index::Var(idx.clone())
            };
            a.belongs.push(index);

            let t = tyenv.get(&arr).unwrap().clone().decay();

            // エイリアス不明の変数で型が一致するものはキャッシュを消去
            if let Some(arr_saved) = unknown_saved.get_mut(&t) {
                arr_saved.clear();
            }
            // エイリアスが分かっている変数は共通部分がありうるもののみ消去
            saved.retain(|a2, _| !may_overlap(&a, a2));

            ArrayPut(arr, idx, x)
        }
        ArrayPut(arr, idx, x) => {
            let t = tyenv.get(&arr).unwrap().clone().decay();

            // 型が一致するものはキャッシュを消去
            if let Some(arr_saved) = unknown_saved.get_mut(&t) {
                arr_saved.clear();
            }
            saved.retain(|_, arr| tyenv.get(arr).unwrap().clone().decay() != t);

            ArrayPut(arr, idx, x)
        }
        TupleGet(tup, idx) => conv_tuple_get(tup, idx, None, aliases, saved),
        Loop { vars, init, body } => {
            cleanse_cache(&body, consts, tyenv, aliases, saved, unknown_saved);

            let body = conv(body, consts, tyenv, aliases, saved, unknown_saved);
            Loop { vars, init, body }
        }
        DoAll {
            idx,
            range,
            delta,
            body,
        } => {
            cleanse_cache(&body, consts, tyenv, aliases, saved, unknown_saved);

            let body = conv(body, consts, tyenv, aliases, saved, unknown_saved);
            DoAll {
                idx,
                range,
                delta,
                body,
            }
        }
        _ => e.item,
    };

    e
}

pub fn eliminate_get(mut p: Program, use_strict_aliasing: bool) -> Program {
    let consts = crate::common::collect_consts(&p);
    let aliases = crate::alias::analyze_aliases(&p, use_strict_aliasing);

    let mut saved = Saved::default();
    let mut unknown_saved = SavedUnknown::default();
    {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(&mut p.global_init, &mut buf);
        p.global_init = conv(
            buf,
            &consts,
            &p.tyenv,
            &aliases,
            &mut saved,
            &mut unknown_saved,
        );
    }
    p.main = conv(
        p.main,
        &consts,
        &p.tyenv,
        &aliases,
        &mut saved,
        &mut unknown_saved,
    );

    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(
            buf,
            &consts,
            &p.tyenv,
            &aliases,
            &mut Saved::default(),
            &mut SavedUnknown::default(),
        );
    }

    p
}
