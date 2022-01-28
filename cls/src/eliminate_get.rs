use ast::closure::*;
use util::{Id, Map, Set, ToSpanned};

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
        CallDir(_, xs) | Tuple(xs) | MakeCls(_, xs) => {
            for x in xs {
                if immut_global.contains(x) {
                    immut_global.remove(x);
                }
            }
        }
        // クロージャ呼び出しは追跡が難しいので存在したらグローバル変数をすべて変更しうるとする
        CallCls(..) => {
            immut_global.clear();
        }
        e => e.map_ref(|e| retain_immut_global_simple(e, immut_global)),
    }
}

fn retain_immut_global_compound(
    e: &Expr,
    current_immut: &Map<Label, Set<Id>>,
    immut_global: &mut Set<Id>,
) {
    use ExprKind::*;
    match &e.item {
        CallDir(label, _) => {
            // 呼び出し先の関数でも変更されないものだけを変更されないとする
            // 外部関数呼び出しでは全て変更されうる
            immut_global.retain(|x| current_immut.get(label).map_or(false, |set| set.contains(x)))
        }
        e => e.map_ref(|e| retain_immut_global_compound(e, current_immut, immut_global)),
    }
}

fn collect_global_invalidation(p: &Program, independents: &Set<Id>) -> Map<Label, Set<Id>> {
    let base: Set<_> = p
        .globals
        .iter()
        .filter(|x| independents.contains(*x))
        .cloned()
        .collect();

    let mut simple_immut = Map::default();
    let mut prev = Map::default();
    for Fundef { name, body, .. } in &p.fundefs {
        let mut immut_global = base.clone();

        retain_immut_global_simple(body, &mut immut_global);

        simple_immut.insert(Label(name.clone()), immut_global);
        prev.insert(Label(name.clone()), Set::default());
    }

    let mut current = simple_immut;
    
    for _ in 0..p.fundefs.len() {
        for Fundef { name, body, .. } in &p.fundefs {
            retain_immut_global_compound(
                body,
                &prev,
                current.get_mut(&Label(name.clone())).unwrap(),
            );
        }

        if prev == current {
            break;
        }

        prev = current.clone();
    }

    for (_, set) in &mut current {
        // 「変更されないグローバル変数」を保存していたのを「変更されうるグローバル変数」に反転
        *set = p
            .globals
            .iter()
            .filter(|x| !set.contains(*x))
            .cloned()
            .collect();
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
    infected_globals_byfunc: Map<Label, Set<Id>>,
}

fn conv_array_get<'a>(
    arr: Id,
    idx: Id,
    v: Option<Id>,
    params: &Parameters<'a>,
    disabled: &Set<Id>,
    indep_saved: &mut Saved,
    unknown_saved: &mut SavedUnknown,
) -> ExprKind {
    use ExprKind::*;

    let index = if let Some(ConstKind::CInt(s)) = params.consts.get(&idx) {
        alias::Index::Int(s.abs() as usize)
    } else {
        alias::Index::Var(idx.clone())
    };

    if let Some(a) = params.aliases.get(&arr) {
        if disabled.contains(&arr) || a.len() != 1 {
            // エイリアスが無効になっているか
            // エイリアスの候補が複数ある場合は諦める (saved を消したりはしない)
            ArrayGet(arr, idx)
        } else {
            assert!(a.len() == 1);
            let mut a = a[0].clone();
            a.belongs.push(index);

            if let Some(x) = indep_saved.get(&a) {
                Var(x.clone())
            } else {
                v.map(|v| indep_saved.insert(a.clone(), v.clone()));
                ArrayGet(arr, idx)
            }
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

fn conv_tuple_get<'a>(
    tup: Id,
    idx: usize,
    v: Option<Id>,
    params: &Parameters<'a>,
    indep_saved: &mut Saved,
) -> ExprKind {
    use ExprKind::*;
    let index = alias::Index::Int(idx);

    if params.independents.contains(&tup) {
        // independent なときだけ最適化
        let mut a = params.aliases.get(&tup).unwrap().clone();
        for a in &mut a {
            a.belongs.push(index.clone());
        }

        if a.len() == 1 {
            let a = a.drain(0..1).next().unwrap();

            if let Some(x) = indep_saved.get(&a) {
                Var(x.clone())
            } else {
                v.map(|v| indep_saved.insert(a, v.clone()));
                TupleGet(tup, idx)
            }
        } else {
            // エイリアスの候補が複数ある場合は諦める
            TupleGet(tup, idx)
        }
    } else {
        TupleGet(tup, idx)
    }
}

fn disable_dep<'a>(
    x: &Id,
    params: &Parameters<'a>,
    disabled: &mut Set<Id>,
    indep_saved: &mut Saved,
) {
    assert!(!params.independents.contains(x));
    if let Some(a) = params.aliases.get(x) {
        // 変数のムーブ: dependent alias を disable してキャッシュを一旦削除
        for a in a {
            indep_saved.remove(a);
        }

        disabled.insert(x.clone());
    }
}

fn conv_array_put<'a>(
    arr: Id,
    idx: Id,
    x: Id,
    v: Option<Id>,
    params: &Parameters<'a>,
    disabled: &mut Set<Id>,
    indep_saved: &mut Saved,
    unknown_saved: &mut SavedUnknown,
) -> ExprKind {
    use ExprKind::*;

    disable_dep(&x, params, disabled, indep_saved);

    let index = if let Some(ConstKind::CInt(s)) = params.consts.get(&idx) {
        alias::Index::Int(s.abs() as usize)
    } else {
        alias::Index::Var(idx.clone())
    };

    if params.aliases.contains_key(&arr) && !disabled.contains(&arr) {
        // independent もしくはまだ disabled でない dependent alias
        let mut a = params.aliases.get(&arr).unwrap().clone();
        for a in &mut a {
            a.belongs.push(index.clone());

            // エイリアスが分かっている変数は共通部分がありうるもののみ消去
            indep_saved.retain(|a2, _| !may_overlap(&a, a2));
        }

        if a.len() == 1 {
            // 候補が一つだけならキャッシュを上書き
            let a = a.drain(0..1).next().unwrap();
            v.map(|v| indep_saved.insert(a, v));
        }

        ArrayPut(arr, idx, x)
    } else {
        // disabled dependent alias または unknown array
        let t = params.tyenv.get(&arr).unwrap().clone().decay();

        // 型が一致するものはキャッシュを消去
        if let Some(arr_saved) = unknown_saved.get_mut(&t) {
            arr_saved.clear();
        }

        v.map(|v| {
            let m = unknown_saved.entry(t).or_default();
            m.insert((arr.clone(), index), v);
        });

        ArrayPut(arr, idx, x)
    }
}

// independent alias : 書き込みがあったら該当キャッシュ消去
// dependent alias : 書き込みがあったら該当キャッシュ消去, move があったら disable にして該当キャッシュ消去
// unknown cache : 同じ型に対する書き込みがあった時点でキャッシュ消去
fn cleanse_cache<'a>(
    e: &Expr,
    params: &Parameters<'a>,
    disabled: &mut Set<Id>,
    indep_saved: &mut Saved,
    unknown_saved: &mut SavedUnknown,
) {
    use ExprKind::*;
    match &e.item {
        ArrayPut(arr, idx, x) => {
            // キャッシュの上書きをしないので `v` は `None`
            conv_array_put(
                arr.clone(),
                idx.clone(),
                x.clone(),
                None,
                params,
                disabled,
                indep_saved,
                unknown_saved,
            );
        }
        Var(x) | AllocArray(_, _, Some(x)) => {
            disable_dep(&x, params, disabled, indep_saved);
        }
        Tuple(xs) | MakeCls(_, xs) => {
            for x in xs {
                disable_dep(x, params, disabled, indep_saved);
            }
        }
        Continue(ps) => {
            for (_, x) in ps {
                disable_dep(x, params, disabled, indep_saved);
            }
        }
        Assign(label, x) => {
            if &label.0 != x {
                disable_dep(x, params, disabled, indep_saved);
            }
        }
        Loop { init, body, .. } => {
            for x in init {
                disable_dep(x, params, disabled, indep_saved);
            }
            cleanse_cache(body, params, disabled, indep_saved, unknown_saved);
        }
        // DoAll は init に配列が入り得ないのでやらなくて良い
        e => e.map_ref(|e| cleanse_cache(e, params, disabled, indep_saved, unknown_saved)),
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
            let mut d1 = disabled.clone();
            let mut s1 = indep_saved.clone();
            let mut us1 = unknown_saved.clone();
            let d2 = disabled;
            let mut s2 = indep_saved.clone();
            let mut us2 = unknown_saved.clone();

            let e1 = conv(e1, params, &mut d1, &mut s1, &mut us1);
            let e2 = conv(e2, params, d2, &mut s2, &mut us2);

            // if の両方の分岐で生き残っているキャッシュのみ収集
            d2.retain(|x| !d1.contains(x));

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
                    disabled,
                    indep_saved,
                    unknown_saved,
                ),
                AllocArray(num, t, Some(init)) => {
                    disable_dep(&init, params, disabled, indep_saved);
                    if let Some(a) = params.aliases.get(&v) {
                        if a.len() == 1 {
                            let a = &a[0];
                            if let Some(ConstKind::CInt(s)) = params.consts.get(&num) {
                                let s = s.abs() as usize;

                                for i in 0..s {
                                    let index = alias::Index::Int(i);
                                    let mut a = a.clone();
                                    a.belongs.push(index);
                                    indep_saved.insert(a, v.clone());
                                }
                            }
                        }
                    }
                    
                    AllocArray(num, t, Some(init))
                }
                ArrayPut(arr, idx, x) => conv_array_put(
                    arr,
                    idx,
                    x,
                    Some(v.clone()),
                    params,
                    disabled,
                    indep_saved,
                    unknown_saved,
                ),
                TupleGet(tup, idx) => {
                    conv_tuple_get(tup, idx, Some(v.clone()), params, indep_saved)
                }
                _ => {
                    conv(
                        Box::new(e1.item.with_span(e1.loc)),
                        params,
                        disabled,
                        indep_saved,
                        unknown_saved,
                    )
                    .item
                }
            };

            let e2 = conv(e2, params, disabled, indep_saved, unknown_saved);
            Let(v, e1, e2)
        }
        CallDir(label, args) => {
            // グローバル変数にクロージャーが入っており, そのクロージャーがコールされる場合は, `infected_globals` に全てのグローバル変数が追加されるので良い
            for x in &args {
                // もし引数にクロージャが入っていたら dependent alias と unknown を全て削除
                let t = params.tyenv.get(x).unwrap();
                if t.has_func() {
                    indep_saved.retain(|_, y| {
                        if !params.independents.contains(y) {
                            disabled.insert(y.clone());
                            false
                        } else {
                            true
                        }
                    });
                    unknown_saved.clear();
                }

                // 引数に含まれる変数を削除
                if t.is_array() || t.is_tuple() {
                    if let Some(a) = params.aliases.get(x) {
                        for a in a {
                            indep_saved.remove(a);
                        }
                    }

                    if let Some(arr_saved) = unknown_saved.get_mut(&t) {
                        arr_saved.clear();
                    }
                }
            }

            // 変更を受けるグローバル変数を全てキャッシュから削除
            if let Some(infected_globals) = params.infected_globals_byfunc.get(&label) {
                for infected in infected_globals {
                    if let Some(a) = params.aliases.get(infected) {
                        for a in a {
                            indep_saved.remove(a);
                        }
                    }
    
                    let t = params.tyenv.get(infected).unwrap();
                    if let Some(arr_saved) = unknown_saved.get_mut(&t) {
                        arr_saved.clear();
                    }
                }
            }
            else {
                // 外部関数呼び出し
                indep_saved.clear();
                unknown_saved.clear();
            }
            

            CallDir(label, args)
        }
        CallCls(f, args) => {
            indep_saved.clear();
            unknown_saved.clear();
            CallCls(f, args)
        }
        ArrayGet(arr, idx) => {
            conv_array_get(arr, idx, None, params, disabled, indep_saved, unknown_saved)
        }
        ArrayPut(arr, idx, x) => conv_array_put(
            arr,
            idx,
            x,
            None,
            params,
            disabled,
            indep_saved,
            unknown_saved,
        ),
        Var(x) if params.tyenv.get(&x).unwrap().is_array() => {
            disable_dep(&x, params, disabled, indep_saved);
            Var(x)
        }
        Tuple(xs) => {
            for x in &xs {
                disable_dep(x, params, disabled, indep_saved);
            }

            Tuple(xs)
        }
        AllocArray(num, t, Some(x)) => {
            disable_dep(&x, params, disabled, indep_saved);

            AllocArray(num, t, Some(x))
        }
        Continue(ps) => {
            for (_, x) in &ps {
                disable_dep(x, params, disabled, indep_saved);
            }
            Continue(ps)
        }
        MakeCls(label, xs) => {
            for x in &xs {
                disable_dep(x, params, disabled, indep_saved);
            }

            MakeCls(label, xs)
        }
        Assign(label, x) => {
            if label.0 != x {
                disable_dep(&x, params, disabled, indep_saved);
            }

            Assign(label, x)
        }
        TupleGet(tup, idx) => conv_tuple_get(tup, idx, None, params, indep_saved),
        Loop { vars, init, body } => {
            for x in &init {
                disable_dep(x, params, disabled, indep_saved);
            }
            cleanse_cache(&body, params, disabled, indep_saved, unknown_saved);

            let body = conv(
                body,
                params,
                disabled,
                &mut indep_saved.clone(),
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
            cleanse_cache(&body, params, disabled, indep_saved, unknown_saved);

            let body = conv(
                body,
                params,
                disabled,
                &mut indep_saved.clone(),
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
    let (independents, aliases) = crate::alias::analyze_aliases(&p, use_strict_aliasing);
    let infected_globals_byfunc = collect_global_invalidation(&p, &independents);

    let params = Parameters {
        tyenv: &p.tyenv,
        consts,
        independents,
        aliases,
        infected_globals_byfunc,
    };

    let mut disabled = Set::default();
    let mut indep_saved = Saved::default();
    let mut unknown_saved = SavedUnknown::default();

    {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(&mut p.global_init, &mut buf);
        p.global_init = conv(
            buf,
            &params,
            &mut disabled,
            &mut indep_saved,
            &mut unknown_saved,
        );
    }
    for Fundef { body, .. } in &mut p.fundefs {
        let mut buf = Box::new(ExprKind::dummy());
        std::mem::swap(body, &mut buf);
        *body = conv(
            buf,
            &params,
            &mut disabled.clone(),
            &mut indep_saved.clone(),
            &mut unknown_saved.clone(),
        );
    }

    p.main = conv(
        p.main,
        &params,
        &mut disabled,
        &mut indep_saved,
        &mut unknown_saved,
    );

    p
}
