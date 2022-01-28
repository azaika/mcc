use ast::closure::*;
use util::{Id, Set, Map, ToSpanned};

use crate::alias::AliasMap;
use crate::alias::{self, may_overlap};
use crate::common::ConstMap;

// immut_global には independent なものしか来ないと仮定
fn retain_immut_global_simple(e: &Expr, immut_global: &mut Set<Id>) {
    use ExprKind::*;
    match &e.item {
        ArrayPut(x, _, _) if immut_global.contains(x) => {
            immut_global.remove(x);
        }
        MakeCls(_, xs) => {
            for x in xs {
                if immut_global.contains(x) {
                    immut_global.remove(x);
                }
            }
        }
        CallDir(_, xs) => {
            for x in xs {
                if immut_global.contains(x) {
                    immut_global.remove(x);
                }
            }
        }
        // クロージャ呼び出しは追跡が難しいので存在したらグローバル変数をすべて変更しうるとする
        CallCls(_, xs) => {
            immut_global.clear();
        }
        e => e.map_ref(|e| retain_immut_global_simple(e, immut_global))
    }
}

fn retain_immut_global_compound(e: &Expr, current_immut: &Map<Label, Set<Id>>, immut_global: &mut Set<Id>) {
    use ExprKind::*;
    match &e.item {
        CallDir(label, _) => {
            // 呼び出し先の関数でも変更されないものだけを変更されないとする
            immut_global.retain(|x| current_immut.get(label).unwrap().contains(x))
        },
        e => e.map_ref(|e| retain_immut_global_compound(e, current_immut, immut_global))
    }
}

fn collect_global_invalidation(p: &Program, independent: &Set<Id>) -> Map<Label, Set<Id>> {
    let base: Set<_> = p.globals.iter().filter(|x| independent.contains(*x)).cloned().collect();

    let mut simple_immut = Map::default();
    for Fundef { name, body, .. } in &p.fundefs {
        let mut immut_global = base.clone();

        retain_immut_global_simple(body, &mut immut_global);

        simple_immut.insert(Label(name.clone()), immut_global);
    }

    let mut current = simple_immut;
    let mut prev = Map::default();
    for _ in 0..p.fundefs.len() {
        for Fundef { name, body, .. } in &p.fundefs {
            retain_immut_global_compound(body, &prev, current.get_mut(&Label(name.clone())).unwrap());
        }

        if prev == current {
            break;    
        }

        prev = current;
    }

    for (_, set) in &mut current {
        // 「変更されないグローバル変数」を保存していたのを「変更されうるグローバル変数」に反転
        *set = p.globals.iter().filter(|x| !set.contains(*x)).cloned().collect();
    }

    current
}

type Saved = Map<alias::Alias, Id>;
type SavedUnknown = Map<Ty, Map<(Id, alias::Index), Id>>;

struct Parameters<'a> {
    tyenv: &'a TyMap,
    consts: ConstMap,
    independents: Set<Id>,
    aliases: AliasMap,
    infected_globals_byfunc: Map<Label, Set<Id>>
}

fn conv_array_get<'a>(
    arr: Id,
    idx: Id,
    v: Option<Id>,
    params: &Parameters<'a>,
    indep_saved: &mut Saved,
    unknown_saved: &mut SavedUnknown,
) -> ExprKind {
    use ExprKind::*;

    let index = if let Some(ConstKind::CInt(s)) = params.consts.get(&idx) {
        alias::Index::Int(s.abs() as usize)
    } else {
        alias::Index::Var(idx.clone())
    };

    if let Some(mut a) = params.aliases.get(&arr).cloned() {
        if a.len() == 1 {
            let a = a[0];
            if let Some(x) = indep_saved.get(&a) {
                Var(x.clone())
            }
            else {
                v.map(|v| indep_saved.insert(a.clone(), v.clone()));
                ArrayGet(arr, idx)
            }
        }
        else {
            // エイリアスの候補が複数ある場合は諦める (saved を消したりはしない)
            ArrayGet(arr, idx)
        }
    } else {
        let t = params.tyenv.get(&arr).unwrap().clone().decay();
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

fn conv_array_put(
    arr: Id,
    idx: Id,
    x: Id,
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
    if aliases.contains_key(&arr) {
        let mut a = aliases.get(&arr).unwrap().clone();

        a.belongs.push(index);

        let t = tyenv.get(&arr).unwrap().clone().decay();

        // エイリアス不明の変数で型が一致するものはキャッシュを消去
        if let Some(arr_saved) = unknown_saved.get_mut(&t) {
            arr_saved.clear();
        }
        // エイリアスが分かっている変数は共通部分がありうるもののみ消去
        saved.retain(|a2, _| !may_overlap(&a, a2));

        v.map(|v| saved.insert(a, v));

        ArrayPut(arr, idx, x)
    } else {
        let t = tyenv.get(&arr).unwrap().clone().decay();

        // 型が一致するものはキャッシュを消去
        if let Some(arr_saved) = unknown_saved.get_mut(&t) {
            arr_saved.clear();
        }
        saved.retain(|_, arr| tyenv.get(arr).unwrap().clone().decay() != t);

        v.map(|v| {
            let mut m = util::Map::default();
            m.insert((arr.clone(), index), v);
            unknown_saved.entry(t).or_insert(m);
        });

        ArrayPut(arr, idx, x)
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

fn conv<'a>(
    mut e: Box<Expr>,
    params: &Parameters<'a>,
    disabled: &mut Set<Id>,
    indep_saved: &mut Saved,
    unknown_saved: &mut SavedUnknown,
) -> Box<Expr> {
    use ExprKind::*;

    e.item = match e.item {
        If(kind, x, y, e1, e2) => {
            let d1 = disabled.clone();
            let mut s1 = indep_saved.clone();
            let mut us1 = unknown_saved.clone();
            let d2 = disabled;
            let mut s2 = indep_saved.clone();
            let mut us2 = unknown_saved.clone();

            let e1 = conv(e1, params, &mut d1, &mut s1, &mut us1);
            let e2 = conv(e2, params, d2, &mut s2, &mut us2);

            // if の両方の分岐で生き残っているキャッシュのみ収集
            d2.retain(|x| !d1.contains(x));
            disabled = d2;

            *indep_saved = s1
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
                    params,
                    indep_saved,
                    unknown_saved,
                ),
                ArrayPut(arr, idx, x) => conv_array_put(
                    arr,
                    idx,
                    x,
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
                        globals,
                        consts,
                        tyenv,
                        aliases,
                        saved,
                        unknown_saved,
                    )
                    .item
                }
            };

            let e2 = conv(e2, globals, consts, tyenv, aliases, saved, unknown_saved);
            Let(v, e1, e2)
        }
        CallDir(label, args) => {
            // グローバル変数にクロージャーが入っており, そのクロージャーがコールされる場合は, `infected_globals` に全てのグローバル変数が追加されるので良い
            // 一方でもし引数にクロージャが入っていると違法な最適化をしてしまう
            for x in globals.iter().chain(&args) {
                saved.retain(|a, _| &a.top != x);
            }

            for x in &args {
                let t = tyenv.get(x).unwrap().clone().decay();
                if t.is_array() {
                    if let Some(a) = aliases.get(x) {
                        saved.retain(|a2, _| may_overlap(a, a2));
                    } else {
                        saved.retain(|_, arr| tyenv.get(arr).unwrap().clone().decay() != t);
                    }
                }
            }

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
        ArrayPut(arr, idx, x) => conv_array_put(
            arr,
            idx,
            x,
            None,
            consts,
            tyenv,
            aliases,
            saved,
            unknown_saved,
        ),
        TupleGet(tup, idx) => conv_tuple_get(tup, idx, None, aliases, saved),
        Loop { vars, init, body } => {
            cleanse_cache(&body, consts, tyenv, aliases, saved, unknown_saved);

            let body = conv(
                body,
                globals,
                consts,
                tyenv,
                aliases,
                &mut saved.clone(),
                &mut unknown_saved.clone(),
            );
            Loop { vars, init, body }
        }
        DoAll {
            idx,
            range,
            delta,
            body,
        } => {
            cleanse_cache(&body, consts, tyenv, aliases, saved, unknown_saved);

            let body = conv(
                body,
                globals,
                consts,
                tyenv,
                aliases,
                &mut saved.clone(),
                &mut unknown_saved.clone(),
            );
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
    let globals = p
        .globals
        .iter()
        .filter_map(|g| {
            p.tyenv
                .get_key_value(g)
                .filter(|(_, t)| t.is_array())
                .map(|(x, _)| x.clone())
        })
        .collect();

    let mut saved = Saved::default();
    let mut unknown_saved = SavedUnknown::default();
    {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(&mut p.global_init, &mut buf);
        p.global_init = conv(
            buf,
            &globals,
            &consts,
            &p.tyenv,
            &aliases,
            &mut saved,
            &mut unknown_saved,
        );
    }
    p.main = conv(
        p.main,
        &globals,
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
            &globals,
            &consts,
            &p.tyenv,
            &aliases,
            &mut Saved::default(),
            &mut SavedUnknown::default(),
        );
    }

    p
}
