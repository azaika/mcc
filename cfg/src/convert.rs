use ast::{closure, mir};
use ty::mir::Ty;
use util::{id, Id, ToSpanned};

fn conv_simple(e: closure::ExprKind) -> mir::InstKind {
    use mir::InstKind;
    match e {
        closure::ExprKind::Const(c) => {
            let c = match c {
                closure::ConstKind::CUnit => mir::ConstKind::CUnit,
                closure::ConstKind::CInt(i) => i.into(),
                closure::ConstKind::CFloat(x) => x.into(),
            };

            InstKind::Const(c)
        }
        closure::ExprKind::Var(v) => InstKind::Var(v),
        closure::ExprKind::UnOp(kind, x) => {
            let kind = match kind {
                closure::UnOpKind::Neg => mir::UnOpKind::Neg,
                closure::UnOpKind::FNeg => mir::UnOpKind::FNeg,
            };

            InstKind::UnOp(kind, x)
        }
        closure::ExprKind::BinOp(kind, x, y) => {
            use mir::BinOpKind::*;
            let kind = match kind {
                closure::BinOpKind::Add => Add,
                closure::BinOpKind::Sub => Sub,
                closure::BinOpKind::FAdd => FAdd,
                closure::BinOpKind::FSub => FSub,
                closure::BinOpKind::Mul => Mul,
                closure::BinOpKind::Div => Div,
                closure::BinOpKind::FMul => FMul,
                closure::BinOpKind::FDiv => FDiv,
            };

            InstKind::BinOp(kind, x, y)
        }
        closure::ExprKind::Tuple(xs) => InstKind::Tuple(xs),
        closure::ExprKind::CallDir(l, args) => InstKind::CallDir(mir::Label(l.0), args),
        closure::ExprKind::CallCls(f, args) => InstKind::CallCls(f, args),
        closure::ExprKind::ArrayGet(x, y) => InstKind::ArrayGet(x, y),
        closure::ExprKind::ArrayPut(x, y, z) => InstKind::ArrayPut(x, y, z),
        closure::ExprKind::TupleGet(x, i) => InstKind::TupleGet(x, i),
        closure::ExprKind::MakeCls(l, xs) => InstKind::MakeCls(mir::Label(l.0), xs),
        _ => panic!("non-simple ExprKind has been passed: {}", e),
    }
}

fn gen_array_init(
    name: Id,
    num: Id,
    init: Id,
    span: util::Span,
    p: &mut mir::Program,
    bid: mir::BlockId,
) -> mir::BlockId {
    use mir::InstKind;
    use mir::TailKind;
    // `let a = Array.make num init` を
    // ```
    // let a = Array.alloc num in
    // for i in 0..len(a) {
    //   a[i] := init
    // }
    // ```
    // に変換する
    let t = p.tymap.get(&init).unwrap();
    p.block_arena[bid].body.push((
        Some(name.clone()),
        InstKind::AllocArray(num.clone(), t.clone()).with_span(span),
    ));

    let idx_var = id::gen_uniq_with("Idx");
    let idx_t = Ty::Mut(Box::new(Ty::Int));

    p.tymap.insert(idx_var.clone(), idx_t);

    let loop_id = p
        .block_arena
        .alloc(mir::Block::with_name(id::gen_uniq_with(".FEEntry")));
    let body_id = p
        .block_arena
        .alloc(mir::Block::with_name(id::gen_uniq_with(".FEBody")));
    let cont_id = p.block_arena.alloc(mir::Block::new());

    *p.block_arena[bid].tail = TailKind::Jump(loop_id).with_span(span);

    let bl = &mut p.block_arena[body_id];
    bl.body = vec![
        (
            None,
            InstKind::ArrayPut(name.clone(), idx_var.clone(), init.clone()).with_span(span),
        ),
        (
            Some(idx_var.clone()),
            InstKind::Var(init.clone()).with_span(span),
        ),
    ];
    bl.tail = Box::new(TailKind::Jump(loop_id).with_span(span));

    let zero_var = id::gen_uniq_with("Idx");
    p.tymap.insert(zero_var.clone(), Ty::Mut(Box::new(Ty::Int)));
    let one_var = id::gen_uniq_with("Idx");
    p.tymap.insert(one_var.clone(), Ty::Mut(Box::new(Ty::Int)));

    *p.block_arena[loop_id].tail = TailKind::IntLoop {
        idx: idx_var,
        range: (zero_var, num),
        delta: one_var,
        element_wise: vec![name],
        body: body_id,
        cont: cont_id,
    }
    .with_span(span);

    cont_id
}

fn conv_let(
    d: closure::Decl,
    e1: Box<closure::Expr>,
    span: util::Span,
    p: &mut mir::Program,
    bid: id_arena::Id<mir::Block>,
) -> Option<mir::BlockId> {
    use mir::InstKind;
    use mir::TailKind;
    let body = &mut p.block_arena[bid].body;

    // unit 型の変数が必要な場合
    if let closure::ExprKind::Var(x) = e1.item {
        body.push((Some(d.name), InstKind::Var(x).with_span(span)));
        return None;
    } else if e1.item == closure::ExprKind::Const(closure::ConstKind::CUnit) {
        body.push((
            Some(d.name),
            InstKind::Const(mir::ConstKind::CUnit).with_span(span),
        ));
        return None;
    };

    // これ以降は unit 型の変数は不要

    let name = d.name.clone();
    let is_unit = d.t == ty::knormal::Ty::Unit;
    let res = if is_unit { None } else { Some(name.clone()) };

    p.tymap.insert(d.name, d.t.into());

    match e1.item {
        closure::ExprKind::If(_, _, _, _, _) => {
            let cont_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, p, bid, res, TailKind::Jump(cont_id), None);

            return Some(cont_id);
        }
        closure::ExprKind::CreateArray(num, init) => {
            return Some(gen_array_init(name, num, init, span, p, bid));
        }
        closure::ExprKind::Loop { .. } => {
            let cont_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, p, bid, res, TailKind::Jump(cont_id), None);

            return Some(cont_id);
        }
        closure::ExprKind::Let(_, _, _) => panic!("knormal::Expr should be let-flattened"),
        closure::ExprKind::Continue(_) => unreachable!(),
        _ => body.push((res.clone(), conv_simple(e1.item).with_span(span))),
    }

    // no block was created
    None
}

fn conv(
    e: Box<closure::Expr>,
    p: &mut mir::Program,
    bid: id_arena::Id<mir::Block>,
    res: Option<Id>,
    tail: mir::TailKind,
    loop_id: Option<mir::BlockId>,
) {
    use closure::ExprKind;
    use mir::InstKind;
    use mir::TailKind;

    match e.item {
        ExprKind::If(kind, x, y, e1, e2) => {
            let b1_id = p.block_arena.alloc(mir::Block::new());
            let b2_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, p, b1_id, res.clone(), tail.clone(), loop_id);
            conv(e2, p, b2_id, res, tail, loop_id);

            let kind = match kind {
                closure::IfKind::IfEq => mir::IfKind::IfEq,
                closure::IfKind::IfLE => mir::IfKind::IfLE,
            };

            p.block_arena[bid].tail =
                Box::new(TailKind::If(kind, x, y, b1_id, b2_id).with_span(e.loc));
        }
        closure::ExprKind::Let(d, e1, e2) => {
            let bid = conv_let(d, e1, e.loc, p, bid).unwrap_or(bid);

            conv(e2, p, bid, res, tail, loop_id);
        }
        ExprKind::Loop {
            vars,
            init,
            body: e1,
        } => {
            assert!(vars.len() == init.len());

            let body = &mut p.block_arena[bid].body;
            for i in 0..vars.len() {
                let v = vars[i].clone();

                p.tymap.insert(v.name.clone(), Ty::Mut(Box::new(v.t.into())));

                body.push((
                    Some(v.name),
                    InstKind::Var(init[i].clone()).with_span(e.loc),
                ));
            }

            // create loop block and convert
            let loop_id = p
                .block_arena
                .alloc(mir::Block::with_name(id::gen_tmp_var_with(".loop")));
            conv(e1, p, loop_id, res, tail, Some(loop_id));
        }
        ExprKind::Continue(xs) => {
            let body = &mut p.block_arena[bid].body;
            for (v, x) in xs {
                body.push((Some(v), InstKind::Var(x).with_span(e.loc)));
            }

            p.block_arena[bid].tail = Box::new(TailKind::Jump(loop_id.unwrap()).with_span(e.loc));
        }
        ExprKind::CreateArray(num, init) => {
            assert!(res.is_some(), "result cannot have type Unit");
            let res = res.unwrap();

            let bid = gen_array_init(res, num, init, e.loc, p, bid);

            p.block_arena[bid].tail = Box::new(tail.with_span(e.loc));
        }
        _ => {
            let block = &mut p.block_arena[bid];
            block.body.push((res, conv_simple(e.item).with_span(e.loc)));
            block.tail = Box::new(tail.with_span(e.loc));
        }
    }
}

fn init_global(globals: Vec<closure::Global>, p: &mut mir::Program) -> mir::BlockId {
    let mut cur_id = p.entry;
    for g in globals {
        use mir::InstKind::Assign;
        use mir::TailKind::Jump;

        let can_skip = g.t == ty::knormal::Ty::Unit && {
            match g.init.item {
                closure::ExprKind::Const(_) | closure::ExprKind::Var(_) => false,
                _ => true,
            }
        };

        let res = if !can_skip {
            p.globals.push(mir::Label(g.name.0.clone()));
            p.tymap.insert(g.name.0.clone(), g.t.into());
            Some(g.name.0.clone())
        } else {
            None
        };

        let cont_id = p
            .block_arena
            .alloc(mir::Block::with_name(format!(".Init@{}", g.name.0)));
        let span = g.init.loc;
        conv(g.init, p, cur_id, res, Jump(cont_id), None);
        p.block_arena[cont_id].body.push((
            None,
            Assign(mir::Label(g.name.0.clone()), g.name.0).with_span(span),
        ));
        cur_id = cont_id;
    }

    cur_id
}

fn convert_fundef(fundefs: Vec<closure::Fundef>, p: &mut mir::Program) {
    for closure::Fundef {
        fvar,
        args,
        formal_fv,
        body,
    } in fundefs
    {
        // register arguments and formal_fv types
        p.tymap.insert(fvar.name.clone(), fvar.t.clone().into());
        for closure::Decl { name, t } in args.iter().chain(&formal_fv) {
            p.tymap.insert(name.clone(), t.clone().into());
        }

        let entry_id = p
            .block_arena
            .alloc(mir::Block::with_name(format!(".Entry@{}", fvar.name)));

        // register fundef
        p.fundefs.push(mir::Fundef {
            name: mir::Label(fvar.name),
            args: args.into_iter().map(|d| d.name).collect(),
            formal_fv: formal_fv.into_iter().map(|d| d.name).collect(),
            entry: entry_id,
        });

        // load global variables
        for g in &p.globals {
            p.block_arena[entry_id].body.push((
                Some(g.0.clone()),
                mir::InstKind::Load(g.clone()).with_span(body.loc),
            ));
        }

        if let ty::knormal::Ty::Fun(_, rt) = fvar.t {
            let is_unit = rt.as_ref() == &ty::knormal::Ty::Unit;
            if is_unit {
                conv(body, p, entry_id, None, mir::TailKind::Return(None), None);
            } else {
                let ret_var = util::id::gen_tmp_var_with(rt.short());
                p.tymap.insert(ret_var.clone(), (*rt).into());

                conv(
                    body,
                    p,
                    entry_id,
                    Some(ret_var.clone()),
                    mir::TailKind::Return(Some(ret_var.clone())),
                    None,
                );
            }
        } else {
            unreachable!()
        }
    }
}

pub fn convert(prog: closure::Program) -> mir::Program {
    let mut arena = id_arena::Arena::new();
    let entry = arena.alloc(mir::Block::with_name("_min_caml_start".to_string()));
    let mut p = mir::Program::new(arena, entry);

    let entry = init_global(prog.globals, &mut p);
    convert_fundef(prog.fundefs, &mut p);

    conv(
        prog.main,
        &mut p,
        entry,
        None,
        mir::TailKind::Return(None),
        None,
    );

    p
}
