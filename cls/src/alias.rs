use ast::closure::*;
use util::Id;

use crate::common;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Index {
    Int(usize),
    Var(Id)
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
    top: Id,
    belongs: Vec<Index>
}

pub type AliasMap = util::Map<Id, Alias>;

fn analyze_aliases_impl(e: &Expr, consts: &common::ConstMap, aliases: &mut AliasMap) {
    use ExprKind::*;
    match &e.item {
        Let(v, e1, e2) => {
            analyze_aliases_impl(e1, consts, aliases);
            match &e1.item {
                Var(x) if aliases.contains_key(x) => {
                    aliases.insert(v.clone(), aliases.get(x).unwrap().clone());
                },
                AllocArray(..) => {
                    aliases.insert(v.clone(), Alias { top: v.clone(), belongs: vec![] });
                },
                ArrayGet(arr, idx) if aliases.contains_key(arr) => {
                    let mut alias = aliases.get(arr).unwrap().clone();
                    let idx = if let Some(ConstKind::CInt(s)) = consts.get(idx) {
                        if *s < 0 {
                            // negative index access
                            // undefined behavior
                            Index::Var(idx.clone())
                        }
                        else {
                            Index::Int(*s as usize)
                        }
                    }
                    else {
                        Index::Var(idx.clone())
                    };

                    alias.belongs.push(idx);
                    aliases.insert(v.clone(), alias);
                },
                TupleGet(x, idx) if aliases.contains_key(x) => {
                    let mut alias = aliases.get(x).unwrap().clone();
                    alias.belongs.push(Index::Int(*idx));
                    aliases.insert(v.clone(), alias);
                },
                _ => ()
            }

            analyze_aliases_impl(e2, consts, aliases);
        },
        e => e.map_ref(|e| analyze_aliases_impl(e, consts, aliases))
    }
}

pub fn analyze_aliases(p: &Program, use_strict_aliasing: bool) -> AliasMap {
    let consts = common::collect_consts(p);
    let mut aliases = AliasMap::default();
    analyze_aliases_impl(&p.global_init, &consts, &mut aliases);
    for Fundef { body, args, .. } in &p.fundefs {
        if use_strict_aliasing {
            for x in args {
                if p.tyenv.get(x).unwrap().is_array() {
                    aliases.insert(x.clone(), Alias { top: x.clone(), belongs: vec![] });
                }
            }
        }
        analyze_aliases_impl(body, &consts, &mut aliases);
    }
    analyze_aliases_impl(&p.main, &consts, &mut aliases);

    for (x, t) in &p.tyenv {
        if !t.is_array() {
            continue;
        }
        if let Some(alias) = aliases.get(x) {
            log::debug!("`{x}` = {:?}", alias);
        }
        else {
            log::debug!("failed to analyze `{x}` alias.");
        }
    }

    aliases
}