use std::hash::Hash;

use id_arena::Arena;
use petgraph::unionfind::UnionFind;

use super::types::*;
use crate::common::{self, REGS};
use crate::mir::mir::*;
use crate::regalloc::liveness;
use util::Id as Var;
use util::{Map, Set};

type Color = &'static str;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum NodeState {
    PreColored,
    SimplifyWorkset,
    FreezeWorkset,
    SpillWorkset,
    Spilled,
    Coalesced,
    Colored,
    SelectStack,
}

impl NodeState {
    fn is_workset(&self) -> bool {
        use NodeState::*;
        match self {
            SimplifyWorkset | FreezeWorkset | SpillWorkset => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum MoveState {
    Coalesced,
    Constrained,
    Frozen,
    Workset,
    Active,
}

fn set_pop<T: Eq + Hash + Clone>(set: &mut Set<T>) -> Option<T> {
    let i = set.iter().next().cloned();

    if let Some(i) = i {
        set.take(&i)
    } else {
        None
    }
}

fn is_real_adj(states: &Vec<NodeState>, u: usize) -> bool {
    match states[u] {
        NodeState::Coalesced | NodeState::SelectStack => false,
        _ => true,
    }
}

pub struct RegAllocator {
    precolored: Map<usize, Color>,
    coalesced: Set<usize>,
    selected_stack: Vec<usize>,
    move_states: Map<ProgramPoint, MoveState>,
    node_states: Vec<NodeState>,

    simplify_workset: Set<usize>,
    freeze_workset: Set<usize>,
    spill_workset: Set<usize>,

    move_workset: Set<(ProgramPoint, (usize, usize))>,

    edges: Vec<Set<usize>>,
    all_edges: Set<(usize, usize)>,
    degrees: Vec<usize>,

    move_lists: Map<usize, Vec<(ProgramPoint, (usize, usize))>>,
    alias: UnionFind<usize>,
}

impl RegAllocator {
    pub fn new(arena: &Arena<Block>, entry: BlockId, var_idx: &Map<Var, usize>) -> Self {
        let n = var_idx.len();
        let (live, mut precolored, moves) = liveness::analyze(arena, entry, n, var_idx);
        for r in common::REGS {
            let v = format!("%{r}");
            let v = *var_idx.get(&v).unwrap();
            precolored.insert(v, r);
        }

        let mut move_lists: Map<usize, Vec<(ProgramPoint, (usize, usize))>> = Map::default();
        let mut move_states = Map::default();
        let mut move_workset = Set::default();
        for (pp, (u, v)) in moves {
            move_states.insert(pp.clone(), MoveState::Workset);
            move_workset.insert((pp.clone(), (u, v)));
            let l1 = move_lists.entry(u).or_default();
            l1.push((pp.clone(), (u, v)));
            let l2 = move_lists.entry(v).or_default();
            l2.push((pp, (u, v)));
        }

        let mut colored = Map::default();
        for (v, c) in &precolored {
            colored.insert(*v, *c);
        }

        let mut edges = vec![Set::default(); n];
        let mut all_edges = Set::with_capacity_and_hasher(n, util::Hasher::default());
        let mut degrees = vec![0; n];
        for (_, set) in live {
            for u in &set {
                let precolored = precolored.contains_key(u);
                let u = *u;
                for v in &set {
                    all_edges.insert((u, *v));
                    if !precolored {
                        degrees[u] += 1;
                        edges[u].insert(*v);
                    }
                }
            }
        }

        let mut node_states = Vec::with_capacity(n);
        let mut simplify_workset = Set::default();
        let mut freeze_workset = Set::default();
        let mut spill_workset = Set::default();
        for v in 0..n {
            if edges[v].len() >= REGS.len() {
                spill_workset.insert(v);
                node_states.push(NodeState::SpillWorkset);
            } else if move_lists.contains_key(&v) {
                freeze_workset.insert(v);
                node_states.push(NodeState::FreezeWorkset);
            } else {
                simplify_workset.insert(v);
                node_states.push(NodeState::SimplifyWorkset);
            }
        }

        let alias = UnionFind::new(n);

        Self {
            precolored,
            coalesced: Set::default(),
            selected_stack: vec![],
            move_states,
            node_states,

            simplify_workset,
            freeze_workset,
            spill_workset,
            move_workset,

            all_edges,
            edges,
            degrees,
            move_lists,
            alias,
        }
    }

    fn enable_moves(&mut self, u: usize) {
        for u in self.edges[u].iter().chain(&Some(u)).cloned() {
            if !is_real_adj(&self.node_states, u) {
                continue;
            }

            for m in self.actual_moves(u) {
                let state = self.move_states.get_mut(&m.0).unwrap();
                if state == &MoveState::Active {
                    *state = MoveState::Workset;
                    self.move_workset.insert(m.clone());
                }
            }
        }
    }

    fn separate_edge(&mut self, u: usize) {
        assert!(self.degrees[u] > 0);

        let d = self.degrees[u];
        self.degrees[u] -= 1;

        if d == REGS.len() {
            self.enable_moves(u);
            self.spill_workset.remove(&u);
            if self.is_move_related(u) {
                self.node_states[u] = NodeState::FreezeWorkset;
                self.freeze_workset.insert(u);
            } else {
                self.node_states[u] = NodeState::SimplifyWorkset;
                self.simplify_workset.insert(u);
            }
        }
    }

    fn simplify(&mut self) {
        let u = set_pop(&mut self.simplify_workset).unwrap();
        self.selected_stack.push(u);
        let x: Vec<_> = self.edges[u]
            .iter()
            .filter_map(|v| {
                if is_real_adj(&self.node_states, *v) {
                    Some(*v)
                } else {
                    None
                }
            })
            .collect();
        for v in x {
            self.separate_edge(v);
        }
    }

    fn actual_moves(&self, u: usize) -> Vec<(ProgramPoint, (usize, usize))> {
        if let Some(list) = self.move_lists.get(&u) {
            list.iter()
                .filter(|(pp, _)| match self.move_states.get(pp).unwrap() {
                    MoveState::Workset | MoveState::Active => true,
                    _ => false,
                })
                .cloned()
                .collect()
        } else {
            vec![]
        }
    }

    fn is_move_related(&self, u: usize) -> bool {
        if let Some(list) = self.move_lists.get(&u) {
            list.iter()
                .any(|(pp, _)| match self.move_states.get(pp).unwrap() {
                    MoveState::Workset | MoveState::Active => true,
                    _ => false,
                })
        } else {
            false
        }
    }

    fn combine(&mut self, u: usize, v: usize) {
        let b1 = self.freeze_workset.remove(&v);
        let b2 = self.spill_workset.remove(&v);
        assert!(b1 || b2);
        self.node_states[v] = NodeState::Coalesced;
        self.coalesced.insert(v);

        self.alias.union(u, v);

        let unified: Vec<_> = self
            .move_lists
            .get(&u)
            .unwrap()
            .iter()
            .chain(self.move_lists.get(&v).unwrap())
            .cloned()
            .collect();
        *self.move_lists.get_mut(&u).unwrap() = unified.clone();
        *self.move_lists.get_mut(&v).unwrap() = unified;

        let adj_v: Vec<_> = self.edges[v]
            .iter()
            .filter_map(|v| {
                if is_real_adj(&self.node_states, *v) {
                    Some(*v)
                } else {
                    None
                }
            })
            .collect();
        for t in adj_v {
            if t == u || self.all_edges.contains(&(t, u)) {
                continue;
            }
            self.all_edges.insert((t, u));
            self.all_edges.insert((u, t));

            if !self.precolored.contains_key(&t) {
                self.edges[t].insert(u);
                self.degrees[t] += 1;
            }
            if !self.precolored.contains_key(&u) {
                self.edges[u].insert(t);
                self.degrees[u] += 1;
            }

            self.separate_edge(t);
        }

        if self.degrees[u] >= REGS.len() && self.node_states[u] == NodeState::FreezeWorkset {
            self.node_states[u] = NodeState::SpillWorkset;
            assert!(self.freeze_workset.remove(&u));
            self.spill_workset.insert(u);
        }

        if !self.precolored.contains_key(&v)
            && !self.is_move_related(v)
            && self.degrees[v] < REGS.len()
        {
            self.node_states[u] = NodeState::SimplifyWorkset;
            assert!(self.freeze_workset.remove(&u));
            self.simplify_workset.insert(u);
        }
    }

    fn is_conservative(&self, u: usize, v: usize) -> bool {
        let mut k = 0;

        const K: usize = REGS.len();

        for x in self.edges[u].iter().chain(&self.edges[v]) {
            if is_real_adj(&self.node_states, *x) && self.degrees[*x] >= K {
                k += 1;
            }
        }

        k < K
    }

    fn is_ok(&self, v: usize, u: usize) -> bool {
        let mut adj = self.edges[v]
            .iter()
            .filter(|t| is_real_adj(&self.node_states, **t));

        adj.all(|t| {
            self.degrees[*t] < REGS.len()
                || self.precolored.contains_key(t)
                || self.all_edges.contains(&(*t, u))
        })
    }

    fn coalesce(&mut self) {
        let (pp, (x, y)) = set_pop(&mut self.move_workset).unwrap();
        let x = self.alias.find(x);
        let y = self.alias.find(y);
        let (u, v) = if self.precolored.contains_key(&y) {
            (y, x)
        } else {
            (x, y)
        };

        let pu = self.node_states[v] == NodeState::PreColored;
        let pv = self.node_states[v] == NodeState::PreColored;

        const K: usize = REGS.len();

        macro_rules! add_workset {
            ($name: expr) => {
                let u = $name;
                if self.node_states[u] != NodeState::PreColored
                    && !self.is_move_related(u)
                    && self.degrees[u] < K
                {
                    self.node_states[u] = NodeState::SimplifyWorkset;
                    assert!(self.freeze_workset.remove(&u));
                    self.simplify_workset.insert(u);
                }
            };
        }

        let state = self.move_states.get_mut(&pp).unwrap();
        if u == v {
            *state = MoveState::Coalesced;
            add_workset!(u);
        } else if pv || self.all_edges.contains(&(u, v)) {
            *state = MoveState::Constrained;
            add_workset!(u);
            add_workset!(v);
        } else if pu && self.is_ok(v, u) {
            *self.move_states.get_mut(&pp).unwrap() = MoveState::Coalesced;
            self.combine(u, v);
        } else if !pu && self.is_conservative(u, v) {
            *self.move_states.get_mut(&pp).unwrap() = MoveState::Coalesced;
            self.combine(u, v);
        } else {
            *self.move_states.get_mut(&pp).unwrap() = MoveState::Active;
        }
    }

    fn freeze_move(&mut self, u: usize) {
        for (pp, (x, y)) in self.actual_moves(u) {
            let v = if self.alias.equiv(y, u) {
                self.alias.find(x)
            } else {
                self.alias.find(y)
            };

            *self.move_states.get_mut(&pp).unwrap() = MoveState::Frozen;
            if !self.is_move_related(v) && self.degrees[v] < REGS.len() {
                self.node_states[v] = NodeState::SimplifyWorkset;
                assert!(self.freeze_workset.remove(&v));
                self.freeze_workset.insert(v);
            }
        }
    }

    fn freeze(&mut self) {
        let u = set_pop(&mut self.freeze_workset).unwrap();
        self.node_states[u] = NodeState::SimplifyWorkset;
        self.simplify_workset.insert(u);

        self.freeze_move(u)
    }

    fn select_spill(&mut self, costs: &mut Vec<usize>) {
        let (u_idx, u) = costs
            .iter()
            .enumerate()
            .find(|(idx, x)| self.spill_workset.contains(*x))
            .unwrap();
        let u = *u;
        costs.remove(u_idx);

        self.node_states[u] = NodeState::SimplifyWorkset;
        self.spill_workset.remove(&u);
        self.simplify_workset.insert(u);

        self.freeze_move(u)
    }

    pub fn do_alloc(mut self, mut costs: Vec<usize>) -> Result<Map<usize, Color>, Set<usize>> {
        loop {
            if !self.simplify_workset.is_empty() {
                self.simplify();
            } else if !self.move_workset.is_empty() {
                self.coalesce();
            } else if !self.freeze_workset.is_empty() {
                self.freeze();
            } else if !self.spill_workset.is_empty() {
                self.select_spill(&mut costs);
            } else {
                break;
            }
        }

        let mut colored = Map::default();
        let mut spilled = Set::default();
        for (u, c) in self.precolored {
            colored.insert(u, c);
        }
        while !self.selected_stack.is_empty() {
            let u = self.selected_stack.pop().unwrap();
            let mut cand: Set<_> = REGS.iter().collect();
            for v in &self.edges[u] {
                let v = self.alias.find(*v);
                if let Some(c) = colored.get(&v) {
                    cand.remove(c);
                }
            }

            if cand.is_empty() {
                spilled.insert(u);
            } else {
                let c = *cand.iter().next().unwrap();
                colored.insert(u, *c);
            }
        }

        if spilled.is_empty() {
            for u in self.coalesced {
                let w = self.alias.find(u);
                colored.insert(u, colored.get(&w).unwrap());
            }

            Ok(colored)
        } else {
            Err(spilled)
        }
    }
}

fn alloc_impl(
    arena: &mut Arena<Block>,
    tyenv: &mut TyMap,
    entry: BlockId,
    globals: &Set<usize>,
    var_idx: &Map<Var, usize>,
    idx_var: &Map<usize, Var>,
) -> Map<usize, Color> {
    match RegAllocator::new(&arena, entry, &var_idx).do_alloc(vec![]) {
        Ok(colored) => colored,
        Err(spilled) => {
            let spilled = spilled
                .iter()
                .map(|x| idx_var.get(x).unwrap().clone())
                .collect();
            for x in &spilled {
                log::info!("spilling `{x}`...");
            }

            super::spill::insert_save_restore(arena, entry, tyenv, &spilled);

            alloc_impl(arena, tyenv, entry, globals, var_idx, idx_var)
        }
    }
}

pub fn alloc(mut p: &mut Program) -> Map<Var, Map<Var, Color>> {
    todo!()
}
