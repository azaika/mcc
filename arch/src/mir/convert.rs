use super::mir;
use crate::virt::program as virt;

use ast::closure::Label;
use util::{id, Id, ToSpanned};

fn conv_simple(e: virt::ExprKind) -> mir::InstKind {
    use mir::InstKind;
    match e {
        virt::ExprKind::Nop => InstKind::Nop,
        virt::ExprKind::Var(x) => InstKind::Mv(x),
        virt::ExprKind::Li(i) => InstKind::Li(i),
        virt::ExprKind::FLi(x) => InstKind::FLi(x),
        virt::ExprKind::GetLabel(label) => InstKind::GetLabel(label),
        virt::ExprKind::LoadLabel(label) => InstKind::LoadLabel(label),
        virt::ExprKind::UnOp(kind, x) => InstKind::UnOp(kind, x),
        virt::ExprKind::IntOp(kind, x, y) => InstKind::IntOp(kind, x, y),
        virt::ExprKind::FloatOp(kind, x, y) => InstKind::FloatOp(kind, x, y),
        virt::ExprKind::CallDir(l, args) => InstKind::CallDir(l, args),
        virt::ExprKind::CallCls(f, args) => InstKind::CallCls(f, args),
        virt::ExprKind::AllocHeap(x) => InstKind::AllocHeap(x),
        virt::ExprKind::Lw(x, y) => InstKind::Lw(x, y),
        virt::ExprKind::Sw(x, y, z) => InstKind::Sw(x, y, z),
        virt::ExprKind::In => InstKind::In,
        virt::ExprKind::Out(x) => InstKind::Out(x),
        _ => panic!("non-simple ExprKind has been passed: {}", e),
    }
}

fn conv_let(
    v: Option<Id>,
    e1: Box<virt::Expr>,
    span: util::Span,
    tyenv: &virt::TyMap,
    p: &mut mir::Program,
    bid: id_arena::Id<mir::Block>,
) -> Option<mir::BlockId> {
    use mir::TailKind;
    let body = &mut p.block_arena[bid].body;

    // これ以降は unit 型の変数は不要
    let v = if let Some(v) = v {
        let t = tyenv.get(&v).unwrap();
        if t == &virt::Ty::Unit {
            None
        } else {
            p.tymap.insert(v.clone(), t.clone());
            Some(v)
        }
    } else {
        None
    };

    let res = v.clone();
    match e1.item {
        virt::ExprKind::If(..) | virt::ExprKind::IfF(..) => {
            let cont_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, tyenv, p, bid, res, TailKind::Jump(cont_id), None);

            return Some(cont_id);
        }
        virt::ExprKind::Loop { .. } => {
            let cont_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, tyenv, p, bid, res, TailKind::Jump(cont_id), None);

            return Some(cont_id);
        }
        virt::ExprKind::Let(v, _, _) => panic!("virt::Expr should be let-flattened in this point (`{:?}`)", v),
        virt::ExprKind::Continue(_) => unreachable!(),
        _ => body.push((res.clone(), conv_simple(e1.item).with_span(span))),
    }

    // no block was created
    None
}

fn conv(
    e: Box<virt::Expr>,
    tyenv: &virt::TyMap,
    p: &mut mir::Program,
    bid: id_arena::Id<mir::Block>,
    res: Option<Id>,
    tail: mir::TailKind,
    loop_id: Option<mir::BlockId>,
) {
    use mir::InstKind;
    use mir::TailKind;
    use virt::ExprKind;

    match e.item {
        ExprKind::If(kind, x, y, e1, e2) => {
            let b1_id = p.block_arena.alloc(mir::Block::new());
            let b2_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, tyenv, p, b1_id, res.clone(), tail.clone(), loop_id);
            conv(e2, tyenv, p, b2_id, res, tail, loop_id);

            p.block_arena[bid].tail =
                Box::new(TailKind::If(kind, x, y, b1_id, b2_id).with_span(e.loc));
        }
        ExprKind::IfF(kind, x, y, e1, e2) => {
            let b1_id = p.block_arena.alloc(mir::Block::new());
            let b2_id = p.block_arena.alloc(mir::Block::new());

            conv(e1, tyenv, p, b1_id, res.clone(), tail.clone(), loop_id);
            conv(e2, tyenv, p, b2_id, res, tail, loop_id);

            p.block_arena[bid].tail =
                Box::new(TailKind::IfF(kind, x, y, b1_id, b2_id).with_span(e.loc));
        }
        ExprKind::Let(d, e1, e2) => {
            let bid = conv_let(d, e1, e.loc, tyenv, p, bid).unwrap_or(bid);

            conv(e2, tyenv, p, bid, res, tail, loop_id);
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
                let t = tyenv.get(&v).unwrap();

                p.tymap.insert(v.clone(), t.clone());

                match init[i].clone() {
                    mir::Value::Var(x) => {
                        body.push((Some(v), InstKind::Mv(x).with_span(e.loc)));
                    }
                    mir::Value::Imm(x) => {
                        body.push((Some(v), InstKind::Li(x as i32).with_span(e.loc)));
                    }
                }
            }

            // create loop block and convert
            let loop_id = p
                .block_arena
                .alloc(mir::Block::with_name(id::gen_tmp_var_with(".loop")));
            conv(e1, tyenv, p, loop_id, res, tail, Some(loop_id));
        }
        ExprKind::Continue(xs) => {
            let body = &mut p.block_arena[bid].body;
            for (v, x) in xs {
                body.push((Some(v), InstKind::Mv(x).with_span(e.loc)));
            }

            p.block_arena[bid].tail = Box::new(TailKind::Jump(loop_id.unwrap()).with_span(e.loc));
        }
        _ => {
            let block = &mut p.block_arena[bid];
            block.body.push((res, conv_simple(e.item).with_span(e.loc)));
            block.tail = Box::new(tail.with_span(e.loc));
        }
    }
}

fn convert_fundef(fundefs: Vec<virt::Fundef>, tyenv: &virt::TyMap, p: &mut mir::Program) {
    for virt::Fundef {
        name,
        args,
        formal_fv,
        body,
    } in fundefs
    {
        assert!(formal_fv.is_empty());

        // register arguments and formal_fv types
        let ft = tyenv.get(&name).unwrap();
        p.tymap.insert(name.clone(), ft.clone().into());
        for x in args.iter().chain(&formal_fv) {
            let t = tyenv.get(x).unwrap();
            p.tymap.insert(x.clone(), t.clone().into());
        }

        let entry_id = p
            .block_arena
            .alloc(mir::Block::with_name(format!(".Entry@{}", name)));

        // register fundef
        p.fundefs.push(mir::Fundef {
            name: Label(name),
            args,
            formal_fv,
            entry: entry_id,
        });

        if let mir::Ty::Fun(_, rt) = ft {
            let is_unit = rt.as_ref() == &mir::Ty::Unit;
            if is_unit {
                conv(
                    body,
                    tyenv,
                    p,
                    entry_id,
                    None,
                    mir::TailKind::Return(None),
                    None,
                );
            } else {
                let ret_var = util::id::gen_tmp_var_with(rt.short());
                p.tymap.insert(ret_var.clone(), (**rt).clone());

                conv(
                    body,
                    tyenv,
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

pub fn convert(prog: virt::Program) -> mir::Program {
    let mut arena = id_arena::Arena::new();
    let entry = arena.alloc(mir::Block::with_name("_min_caml_start".to_string()));
    let exit = arena.alloc(mir::Block::with_name("_min_caml_end".to_string()));
    let mut p = mir::Program::new(arena, entry, exit);

    convert_fundef(prog.fundefs, &prog.tyenv, &mut p);

    conv(
        prog.main,
        &prog.tyenv,
        &mut p,
        entry,
        None,
        mir::TailKind::Jump(exit),
        None,
    );

    p
}
