use ast::closure::*;
use util::{Id, Map, Set};

use crate::common;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Index {
    Int(usize),
    Var(Id),
}

impl Index {
    pub fn get_int(&self) -> usize {
        match self {
            Index::Int(i) => *i,
            Index::Var(_) => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Alias {
    pub top: Id,
    pub belongs: Vec<Index>,
}

// エイリアス `written` への書き込みが `alias` のキャッシュを無効にしうるか
pub fn may_overlap(written: &Alias, alias: &Alias) -> bool {
    if (written.top != alias.top) || (written.belongs.len() > alias.belongs.len()) {
        return false;
    }

    for (b1, b2) in written.belongs.iter().zip(&alias.belongs) {
        use Index::Int;
        match (b1, b2) {
            (Int(i), Int(j)) if i != j => return false,
            _ => (),
        };
    }
    return true;
}

pub type AliasMap = Map<Id, Vec<Alias>>;

fn iterate_loop(
    e: &Expr,
    consts: &common::ConstMap,
    aliases: &mut AliasMap,
    vars_aliases: &mut Vec<Option<Set<Alias>>>,
) {
    use ExprKind::*;

    match &e.item {
        Continue(ps) => {
            assert!(ps.len() == vars_aliases.len());

            for ((_, x), var_alias) in ps.iter().zip(vars_aliases) {
                if var_alias.is_none() {
                    continue;
                }
                let map = var_alias.as_mut().unwrap();
                if let Some(a) = aliases.get(x) {
                    map.extend(a.clone());
                } else {
                    *var_alias = None;
                }
            }
        }
        Loop { .. } | DoAll { .. } => {
            // stop iterating
        }
        e => e.map_ref(|e| iterate_loop(e, consts, aliases, vars_aliases)),
    }
}

fn analyze_loop(
    vars: &Vec<Id>,
    init: &Vec<Id>,
    body: &Expr,
    consts: &common::ConstMap,
    aliases: &mut AliasMap,
) {
    assert!(vars.len() == init.len());

    let mut vars_aliases: Vec<Option<Set<Alias>>> = vec![None; vars.len()];
    for (map, i) in vars_aliases.iter_mut().zip(init) {
        // 初期化変数がアドレス確定なら追跡
        if let Some(a) = aliases.get(i).clone() {
            let s = a.iter().cloned().collect();
            *map = Some(s);
        }
    }

    for (var, a) in vars.iter().zip(&vars_aliases) {
        if let Some(a) = a {
            aliases.insert(var.clone(), a.iter().cloned().collect());
        }
    }

    let mut old = vars_aliases.clone();
    // 適当な回数で止める
    for _ in 0..10 {
        iterate_loop(body, consts, aliases, &mut vars_aliases);

        for (v, a) in vars.iter().zip(&vars_aliases) {
            if let Some(a) = a {
                aliases.insert(v.clone(), a.iter().cloned().collect());
            }
            else {
                aliases.remove(v);
            }
        }

        if old == vars_aliases {
            // ループ変数が取りうる変数の範囲を定める
            return;
        }

        old = vars_aliases.clone();
    }

    // 一定回数繰り返しても安定しない場合は解析失敗
    // aliases をもとに戻す
    for v in vars {
        aliases.remove(v);
    }
}

// alias: independent であるか、そうではないが CreateArray で作られているもの
fn analyze_aliases_impl(
    e: &Expr,
    consts: &common::ConstMap,
    independent: &Set<Id>,
    aliases: &mut AliasMap,
) {
    use ExprKind::*;
    match &e.item {
        Let(v, e1, e2) => {
            match &e1.item {
                Var(x) if aliases.contains_key(x) => {
                    aliases.insert(v.clone(), aliases.get(x).unwrap().clone());
                }
                AllocArray(..) => {
                    aliases.insert(
                        v.clone(),
                        vec![Alias {
                            top: v.clone(),
                            belongs: vec![],
                        }],
                    );
                }
                ArrayGet(arr, idx) if independent.contains(v) && aliases.contains_key(arr) => {
                    let mut alias = aliases.get(arr).unwrap().clone();
                    let idx = if let Some(ConstKind::CInt(s)) = consts.get(idx) {
                        if *s < 0 {
                            // negative index access
                            // undefined behavior
                            Index::Var(idx.clone())
                        } else {
                            Index::Int(*s as usize)
                        }
                    } else {
                        Index::Var(idx.clone())
                    };

                    for Alias { belongs, .. } in &mut alias {
                        belongs.push(idx.clone());
                    }

                    aliases.insert(v.clone(), alias);
                }
                TupleGet(x, idx) if independent.contains(v) && aliases.contains_key(x) => {
                    let mut alias = aliases.get(x).unwrap().clone();
                    for Alias { belongs, .. } in &mut alias {
                        belongs.push(Index::Int(*idx));
                    }
                    aliases.insert(v.clone(), alias);
                }
                e => e.map_ref(|e| analyze_aliases_impl(e, consts, independent, aliases)),
            }

            analyze_aliases_impl(e2, consts, independent, aliases);
        }
        Loop { vars, init, body } => {
            analyze_loop(vars, init, body, consts, aliases);
            analyze_aliases_impl(body, consts, independent, aliases);
        }
        e => e.map_ref(|e| analyze_aliases_impl(e, consts, independent, aliases)),
    }
}

// independent: 確実に自分自身のエイリアスが存在しない (要素のエイリアスは存在するかもしれない) 変数
fn eliminate_dependent(e: &Expr, tyenv: &TyMap, independent: &mut Set<Id>) {
    use ExprKind::*;
    match &e.item {
        Var(x) | AllocArray(_, _, Some(x)) | ArrayPut(_, _, x) => {
            independent.remove(x);
        }
        Assign(Label(lab), x) if lab != x => {
            independent.remove(x);
            independent.remove(lab);
        }
        Let(v, e1, e2) => {
            match &e1.item {
                ArrayGet(arr, _) if tyenv.get(arr).unwrap().elem_t().is_pointer() => {
                    independent.remove(v);
                }
                TupleGet(tup, _) if tyenv.get(tup).unwrap().is_pointer() => {
                    independent.remove(v);
                }
                Var(_) | If(..) | Let(..) | Loop { .. } | DoAll { .. } | CallDir(..) | CallCls(..) | ExtArray(..) => {
                    independent.remove(v);
                }
                _ => (),
            };

            eliminate_dependent(e1, tyenv, independent);
            eliminate_dependent(e2, tyenv, independent);
        }
        Tuple(xs) | CallDir(_, xs) | CallCls(_, xs) | MakeCls(_, xs) | Asm(_, xs) => {
            for x in xs {
                independent.remove(x);
            }
        }
        Loop { init, body, .. } => {
            for x in init {
                independent.remove(x);
            }
            eliminate_dependent(body, tyenv, independent);
        }
        Continue(ps) => {
            for (_, x) in ps {
                independent.remove(x);
            }
        }
        e => e.map_ref(|e| eliminate_dependent(e, tyenv, independent)),
    }
}

fn collect_independent(p: &Program, use_strict_aliasing: bool) -> Set<Id> {
    let mut independent = Set::default();

    for (x, t) in &p.tyenv {
        if t.is_array() || t.is_tuple() {
            independent.insert(x.clone());
        }
    }

    eliminate_dependent(&p.global_init, &p.tyenv, &mut independent);

    for Fundef { args, body, .. } in &p.fundefs {
        // strict aliasing モードでないときには関数引数は dependent とみなす
        if !use_strict_aliasing {
            for x in args {
                independent.remove(x);
            }
        }

        eliminate_dependent(body, &p.tyenv, &mut independent);
    }

    eliminate_dependent(&p.main, &p.tyenv, &mut independent);

    independent
}

pub fn analyze_aliases(p: &Program, use_strict_aliasing: bool) -> (Set<Id>, AliasMap) {
    let consts = common::collect_consts(p);

    let independent = collect_independent(p, use_strict_aliasing);
    let mut aliases = AliasMap::default();
    analyze_aliases_impl(&p.global_init, &consts, &independent, &mut aliases);
    for Fundef { body, args, .. } in &p.fundefs {
        if use_strict_aliasing {
            for x in args {
                let t = p.tyenv.get(x).unwrap();
                if t.is_array() || t.is_tuple() {
                    aliases.insert(
                        x.clone(),
                        vec![Alias {
                            top: x.clone(),
                            belongs: vec![],
                        }],
                    );
                }
            }
        }
        analyze_aliases_impl(body, &consts, &independent, &mut aliases);
    }
    analyze_aliases_impl(&p.main, &consts, &independent, &mut aliases);

    for (x, t) in &p.tyenv {
        if !t.is_array() {
            continue;
        }
        if let Some(alias) = aliases.get(x) {
            log::debug!("`{x}` = {:?}", alias);
        } else {
            log::debug!("failed to analyze `{x}` alias.");
        }
    }

    (independent, aliases)
}
