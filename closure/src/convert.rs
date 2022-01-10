use util::ToSpanned;
use util::{ id, Id };
use ast::{ knormal, closure };
use ty::knormal::Ty;

type Set = util::Set<Id>;

fn conv_simple(e : knormal::ExprKind, known: &mut Set) -> mir::InstKind {
    use mir::InstKind;
    match e {
        knormal::ExprKind::Const(c) => {
            let c = match c {
                knormal::ConstKind::CUnit => mir::ConstKind::CUnit,
                knormal::ConstKind::CInt(i) => i.into(),
                knormal::ConstKind::CFloat(x) => x.into(),
            };

            InstKind::Const(c)
        },
        knormal::ExprKind::Var(v) => InstKind::Var(v),
        knormal::ExprKind::UnOp(kind, x) => {
            let kind = match kind {
                knormal::UnOpKind::Neg => mir::UnOpKind::Neg,
                knormal::UnOpKind::FNeg => mir::UnOpKind::FNeg,
            };

            InstKind::UnOp(kind, x)
        },
        knormal::ExprKind::BinOp(kind, x, y) => {
            use mir::BinOpKind::*;
            let kind = match kind {
                knormal::BinOpKind::Add => Add,
                knormal::BinOpKind::Sub => Sub,
                knormal::BinOpKind::FAdd => FAdd,
                knormal::BinOpKind::FSub => FSub,
                knormal::BinOpKind::Mul => Mul,
                knormal::BinOpKind::Div => Div,
                knormal::BinOpKind::FMul => FMul,
                knormal::BinOpKind::FDiv => FDiv,
            };

            InstKind::BinOp(kind, x, y)
        },
        knormal::ExprKind::Tuple(xs) => InstKind::Tuple(xs),
        knormal::ExprKind::App(f, args) => {
            if known.contains(&f) {
                log::info!("directly applying {}.", f);
                InstKind::CallDir(mir::Label(f), args)
            }
            else {
                InstKind::CallCls(f, args)
            }
        },
        knormal::ExprKind::ExtApp(f, args) => InstKind::CallDir(mir::Label(f), args),
        knormal::ExprKind::Get(x, y) => InstKind::ArrayGet(x, y),
        knormal::ExprKind::Put(x, y, z) => InstKind::ArrayPut(x, y, z),
        _ => panic!("non-simple ExprKind has been passed: {}", e)
    }
}

fn conv_let(d: knormal::Decl, e1: Box<knormal::Expr>, tyenv: &knormal::TyMap, span: util::Span, p: &mut mir::Program, known: &mut Set, bid: id_arena::Id<mir::Block>, is_top: bool) -> Option<mir::BlockId> {
    use mir::InstKind;
    use mir::TailKind;
    let body = &mut p.block_arena[bid].body;

    // unit 型の変数が必要な場合
    if let knormal::ExprKind::Var(x) = e1.item {
        body.push((Some(d.name), InstKind::Var(x).with_span(span)));
        return None;
    }
    else if e1.item == knormal::ExprKind::Const(knormal::ConstKind::CUnit) {
        body.push((Some(d.name), InstKind::Const(mir::ConstKind::CUnit).with_span(span)));
        return None;
    };

    // これ以降は unit 型の変数は不要

    let name = d.name.clone();
    let is_unit = d.t == ty::knormal::Ty::Unit;
    let res = if is_unit { None } else { Some(name.clone()) };

    // グローバル変数にする
    if is_top {
        todo!()
    }

    match e1.item {
        knormal::ExprKind::If(_, _, _, _, _) => {
            let cont_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, tyenv, p, known, bid, res, TailKind::Jump(cont_id), None, is_top);

            return Some(cont_id);
        },
        knormal::ExprKind::CreateArray(num, init) => {
            return Some(gen_array_init(name, num, init, span, p, bid));
        },
        knormal::ExprKind::Loop { .. } => {
            let cont_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, tyenv, p, known, bid, res, TailKind::Jump(cont_id), None, is_top);

            return Some(cont_id);
        },
        knormal::ExprKind::Let(_) => unreachable!("knormal::Expr should be let-flattened"),
        knormal::ExprKind::Continue(_) => unreachable!(),
        _ => body.push((res.clone(), conv_simple(e1.item, known).with_span(span)))
    }

    // no block was created
    None
}

// func: 自由変数としては現れてはいけないが、関数としては現れて良い関数の集合
fn has_free_impl(func: &mut Set, e: &knormal::Expr, known: &mut Set) -> bool {
    use knormal::ExprKind::*;
    match &e.item {
        Var(x) | UnOp(_, x) => !known.contains(x),
        BinOp(_, x, y) | CreateArray(x, y) | Get(x, y) => !known.contains(x) || !known.contains(y),
        If(_, x, y, e1, e2) => {
            !known.contains(x) || !known.contains(y) || has_free_impl(func, e1, known) || has_free_impl(func, e2, known)
        },
        Let(l) => match l {
            knormal::LetKind::Let(d, e1, e2) => {
                if !known.contains(&d.name) || has_free_impl(func, e1, known) {
                    true
                }
                else {
                    known.insert(d.name.clone());
                    let r = has_free_impl(func, e2, known);
                    known.remove(&d.name);
                    r
                }
            },
            knormal::LetKind::LetRec(fundef, e2) => {
                for knormal::Decl{ name, .. } in &fundef.args {
                    known.insert(name.clone());
                }
                func.insert(fundef.fvar.name.clone());

                let r = has_free_impl(func, &fundef.body, known);
                
                func.remove(&fundef.fvar.name);
                for knormal::Decl{ name, .. } in &fundef.args {
                    known.remove(name);
                }

                r || has_free_impl(func, &e2, known)
            },
            knormal::LetKind::LetTuple(ds, x, e2) => {
                if !known.contains(x) {
                    true
                }
                else {
                    for knormal::Decl{ name, .. } in ds {
                        known.insert(name.clone());
                    }

                    let r = has_free_impl(func, &e2, known);

                    for knormal::Decl{ name, .. } in ds {
                        known.remove(name);
                    }

                    r
                }
            },
        },
        Tuple(xs) | ExtApp(_, xs) => !xs.iter().all(|x| known.contains(x)),
        App(f, args) => {
            (!func.contains(f) && known.contains(f)) || !args.iter().all(|x| known.contains(x))
        },
        Put(x, y, z) => !known.contains(x) || !known.contains(y) || !known.contains(z),
        Loop { vars, loop_vars, init, body } => {
            if !init.iter().all(|x| known.contains(x)) {
                return true;
            }
            
            for knormal::Decl{ name, .. } in vars.iter().chain(loop_vars) {
                known.insert(name.clone());
            }

            let r = has_free_impl(func, &body, known);

            for knormal::Decl{ name, .. } in vars.iter().chain(loop_vars) {
                known.remove(name);
            }

            r
        },
        Continue(xs) => !xs.iter().all(|(_, x)| known.contains(x)),
        Const(_) | ExtArray(_) => false,
    }
}

fn has_free(fundef: &knormal::Fundef, p: &mir::Program, known: &Set) -> bool {
    let mut known = known.clone();
    for mir::Global{ name, .. } in &p.globals {
        known.insert(name.0.clone());
    }
    for knormal::Decl{ name, .. } in &fundef.args {
        known.insert(name.clone());
    }

    let mut s = Set::default();
    s.insert(fundef.fvar.name.clone());
    has_free_impl(&mut s, &fundef.body, &mut known)
}

fn emerge(e: &knormal::Expr, name: &Id) -> bool {
    use knormal::ExprKind::*;
    match &e.item {
        Var(x) | UnOp(_, x) => x == name,
        BinOp(_, x, y) | CreateArray(x, y) | Get(x, y) => x == name || y == name,
        If(_, x, y, e1, e2) => {
            x == name || y == name || emerge(e1, name) || emerge(e2, name)
        },
        Let(l) => match l {
            knormal::LetKind::Let(_, e1, e2) | knormal::LetKind::LetRec(knormal::Fundef { body: e1, .. }, e2) => emerge(e1, name) || emerge(e2, name),
            knormal::LetKind::LetTuple(_, x, e2) => x == name || emerge(e2, name),
        },
        Tuple(xs) | ExtApp(_, xs) | App(_, xs) => xs.iter().any(|x| x == name),
        Put(x, y, z) => x == name || y == name || z == name,
        Loop { init, body, .. } => init.iter().any(|x| x == name) || emerge(body, name),
        Continue(xs) => xs.iter().any(|(_, x)| x == name),
        Const(_) | ExtArray(_) => false,
    }
}

// ToDo: グローバル変数への対応
fn conv(e: Box<knormal::Expr>, tyenv: &knormal::TyMap, p: &mut mir::Program, known: &mut Set, bid: id_arena::Id<mir::Block>, res: Option<Id>, tail: mir::TailKind, loop_id: Option<mir::BlockId>, is_top: bool) {
    use knormal::ExprKind;
    use mir::InstKind;
    use mir::TailKind;

    match e.item {
        ExprKind::If(kind, x, y, e1, e2) => {
            let b1_id = p.block_arena.alloc(mir::Block::new());
            let b2_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, tyenv, p, known, b1_id, res.clone(), tail.clone(), loop_id, is_top);
            conv(e2, tyenv, p, known, b2_id, res, tail, loop_id, is_top);

            let kind = match kind {
                knormal::IfKind::IfEq => mir::IfKind::IfEq,
                knormal::IfKind::IfLE => mir::IfKind::IfLE,
            };

            p.block_arena[bid].tail = Box::new(TailKind::If(kind, x, y, b1_id, b2_id).with_span(e.loc));
        },
        ExprKind::Let(l) => match l {
            knormal::LetKind::Let(d, e1, e2) => {
                let bid = conv_let(d, e1, tyenv, e.loc, p, known, bid, is_top).unwrap_or(bid);

                conv(e2, tyenv, p, known, bid, res, tail, loop_id, is_top);
            },
            knormal::LetKind::LetRec(fundef, e2) => {
                // 自由変数が無いなら自身は CallDir で呼ぶようにする
                if !has_free(&fundef, p, known) {
                    known.insert(fundef.fvar.name.clone());
                }

                let entry_id = p.block_arena.alloc(mir::Block::with_name(format!(".Entry@{}", fundef.fvar.name)));

                // convert function body
                if let ty::knormal::Ty::Fun(_, t) = tyenv.get(&fundef.fvar.name).unwrap() {
                    let is_unit = *t.as_ref() == ty::knormal::Ty::Unit;
                    if is_unit {
                        conv(fundef.body, tyenv, p, known, entry_id, None, TailKind::Return(None), None, false);
                    }
                    else {
                        let ret_var = id::gen_uniq_with(ty::knormal::short(t));
                        conv(fundef.body, tyenv, p, known, entry_id, Some(ret_var.clone()), TailKind::Return(Some(ret_var.clone())), None, false);
                    }
                }
                else {
                    unreachable!()
                }

                // free variables which are contained in converted function body
                let fvs = {
                    let mut known = Set::default();
                    for mir::Global { name, .. } in &p.globals {
                        known.insert(name.0.clone());
                    }
                    for knormal::Decl { name, .. } in &fundef.args {
                        known.insert(name.clone());
                    }

                    p.collect_fv(entry_id, known)
                };
                let fvs : Vec<_> = fvs.into_iter().map(|x| x.clone()).collect();

                // add fundef
                for knormal::Decl { name, t } in &fundef.args {
                    p.tymap.insert(name.clone(), t.clone().into());
                }
                
                p.fundefs.push(mir::Fundef {
                    name: mir::Label(fundef.fvar.name.clone()),
                    args: fundef.args.into_iter().map(|d| d.name).collect(),
                    formal_fv: fvs.iter().map(|x| x.clone()).collect(),
                    entry: entry_id,
                });

                // make closure (if needed)
                if emerge(&e2, &fundef.fvar.name) {
                    let f = fundef.fvar.name.clone();
                    p.tymap.insert(f.clone(), fundef.fvar.t.into());
                    p.block_arena[bid].body.push((Some(f), InstKind::MakeCls(
                        mir::Label(fundef.fvar.name),
                        fvs.into_iter().map(|x| x.clone()).collect()
                    ).with_span(e.loc)));
                }

                // convert following programs
                conv(e2, tyenv, p, known, bid, res, tail, loop_id, is_top);
            },
            knormal::LetKind::LetTuple(ds, x, e2) => {
                // `let (x1, ..., xn) = x in e2` を
                // ```
                // let x1 = x.0 in
                // ...
                // let xn = x.(n-1) in
                // e2
                // ```
                // に変換する
                let body = &mut p.block_arena[bid].body;
                for (idx, knormal::Decl { name, t }) in ds.into_iter().enumerate() {
                    body.push((Some(name.clone()), InstKind::TupleGet(x.clone(), idx).with_span(e.loc)));
                    p.tymap.insert(name, t.into());
                }
                
                conv(e2, tyenv, p, known, bid, res, tail, loop_id, is_top);
            },
        },
        ExprKind::Loop { loop_vars: vars, init, body: e1 } => {
            let body = &mut p.block_arena[bid].body;
            // initialize loop variables and add loop variables to tymap
            let zero_var = id::gen_uniq_with(Ty::Int.short());
            p.tymap.insert(zero_var.clone(), Ty::Int);
            body.push((Some(zero_var.clone()), InstKind::Const(0.into()).with_span(e.loc)));
            let one_var = id::gen_uniq_with(Ty::Int.short());
            p.tymap.insert(one_var.clone(), Ty::Int);
            body.push((Some(one_var.clone()), InstKind::Const(1.into()).with_span(e.loc)));
            for (d, x) in vars.into_iter().zip(init) {
                let t: Ty = d.t.into();
                p.tymap.insert(d.name.clone(), t.clone());
                if let Ty::Array(t, _) = t {
                    body.push((Some(d.name.clone()), InstKind::AllocArray(one_var.clone(), *t).with_span(e.loc)));
                }
                else {
                    unreachable!()
                }

                body.push((None, InstKind::ArrayPut(d.name, zero_var.clone(), x).with_span(e.loc)));
            }

            // create loop block and convert
            let loop_id = p.block_arena.alloc(mir::Block::with_name(id::gen_uniq_with(".loop")));
            conv(e1, tyenv, p, known, loop_id, res, tail, Some(loop_id), false);
        },
        ExprKind::Continue(xs) => {
            let body = &mut p.block_arena[bid].body;
            let zero_var = id::gen_uniq_with(Ty::Int.short());
            p.tymap.insert(zero_var.clone(), Ty::Int);
            body.push((Some(zero_var.clone()), InstKind::Const(0.into()).with_span(e.loc)));
            for (v, x) in xs {
                body.push((None, InstKind::ArrayPut(v, zero_var.clone(), x).with_span(e.loc)));
            }

            p.block_arena[bid].tail = Box::new(TailKind::Jump(loop_id.unwrap()).with_span(e.loc));
        },
        ExprKind::CreateArray(num, init) => {
            assert!(res.is_some(), "result cannot have type Unit");
            let res = res.unwrap();
            
            let bid = gen_array_init(res, num, init, e.loc, p, bid);

            p.block_arena[bid].tail = Box::new(tail.with_span(e.loc));
        },
        _ => {
            let block = &mut p.block_arena[bid];
            block.body.push((res, conv_simple(e.item, known).with_span(e.loc)));
            block.tail = Box::new(tail.with_span(e.loc));
        }
    }
}

fn has_func(e: &knormal::Expr) -> bool {
    match &e.item {
        knormal::ExprKind::If(_, _, _, e1, e2) => has_func(e1) || has_func(e2),
        knormal::ExprKind::Let(l) => match l {
            knormal::LetKind::Let(_, e1, e2) => has_func(e1) || has_func(e2),
            knormal::LetKind::LetRec(_, _) => true,
            knormal::LetKind::LetTuple(_, _, e2) => has_func(e2),
        },
        knormal::ExprKind::Loop { body, .. } => has_func(body),
        _ => false,
    }
}

pub fn convert(e: knormal::Expr, tyenv: knormal::TyMap) -> mir::Program {
    let mut arena = id_arena::Arena::new();
    let entry = arena.alloc(mir::Block::with_name("_min_caml_start".to_string()));
    let mut p = mir::Program::new(arena, entry);

    let is_top = !has_func(&e);
    conv(Box::new(e), &tyenv, &mut p, &mut Set::default(), entry, None, mir::TailKind::Return(None), None, is_top);

    p
}