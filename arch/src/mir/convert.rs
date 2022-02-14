use super::mir;
use crate::{
    common::{self, REGS},
    virt::program as virt,
};

use ast::closure::Label;
use id_arena::Arena;
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
    old_tyenv: &virt::TyMap,
    tyenv: &mut mir::TyMap,
    arena: &mut Arena<mir::Block>,
    bid: id_arena::Id<mir::Block>,
) -> Option<mir::BlockId> {
    use mir::TailKind;
    let body = &mut arena[bid].body;

    // これ以降は unit 型の変数は不要
    let v = if let Some(v) = v {
        let t = old_tyenv.get(&v).unwrap();
        if t == &virt::Ty::Unit {
            None
        } else {
            tyenv.insert(v.clone(), t.clone());
            Some(v)
        }
    } else {
        None
    };

    let res = v.clone();
    match e1.item {
        virt::ExprKind::If(..) | virt::ExprKind::IfF(..) => {
            let cont_id = arena.alloc(mir::Block::new());

            conv(
                e1,
                old_tyenv,
                tyenv,
                arena,
                bid,
                res,
                TailKind::Jump(cont_id),
                None,
            );

            return Some(cont_id);
        }
        virt::ExprKind::Loop { .. } => {
            let cont_id = arena.alloc(mir::Block::new());

            conv(
                e1,
                old_tyenv,
                tyenv,
                arena,
                bid,
                res,
                TailKind::Jump(cont_id),
                None,
            );

            return Some(cont_id);
        }
        virt::ExprKind::Let(v, _, _) => panic!(
            "virt::Expr should be let-flattened in this point (`{:?}`)",
            v
        ),
        virt::ExprKind::Continue(_) => unreachable!(),
        virt::ExprKind::CallDir(label, xs) => {
            if xs.len() > common::REGS.len() {
                panic!("too many arguments!!!");
            }

            for (x, r) in xs.into_iter().zip(common::REGS) {
                let reg = format!("%{}", r);
                let kind = match x {
                    virt::Value::Var(x) => mir::InstKind::Mv(x),
                    virt::Value::Imm(x) => mir::InstKind::Li(x as i32),
                };
                body.push((Some(reg), kind.with_span(span)));
            }

            let ret = format!("%{}", common::REG_RET);
            body.push((
                Some(ret.clone()),
                mir::InstKind::CallDir(label).with_span(span),
            ));
            body.push((res.clone(), mir::InstKind::Mv(ret).with_span(span)));
        }
        virt::ExprKind::CallCls(..) => unimplemented!("I have no time"),
        _ => body.push((res.clone(), conv_simple(e1.item).with_span(span))),
    }

    // no block was created
    None
}

fn conv(
    e: Box<virt::Expr>,
    old_tyenv: &virt::TyMap,
    tyenv: &mut mir::TyMap,
    arena: &mut Arena<mir::Block>,
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
            let b1_id = arena.alloc(mir::Block::new());
            let b2_id = arena.alloc(mir::Block::new());

            conv(
                e1,
                old_tyenv,
                tyenv,
                arena,
                b1_id,
                res.clone(),
                tail.clone(),
                loop_id,
            );
            conv(e2, old_tyenv, tyenv, arena, b2_id, res, tail, loop_id);

            arena[bid].tail = Box::new(TailKind::If(kind, x, y, b1_id, b2_id).with_span(e.loc));
        }
        ExprKind::IfF(kind, x, y, e1, e2) => {
            let b1_id = arena.alloc(mir::Block::new());
            let b2_id = arena.alloc(mir::Block::new());

            conv(
                e1,
                old_tyenv,
                tyenv,
                arena,
                b1_id,
                res.clone(),
                tail.clone(),
                loop_id,
            );
            conv(e2, old_tyenv, tyenv, arena, b2_id, res, tail, loop_id);

            arena[bid].tail = Box::new(TailKind::IfF(kind, x, y, b1_id, b2_id).with_span(e.loc));
        }
        ExprKind::Let(d, e1, e2) => {
            let bid = conv_let(d, e1, e.loc, old_tyenv, tyenv, arena, bid).unwrap_or(bid);

            conv(e2, old_tyenv, tyenv, arena, bid, res, tail, loop_id);
        }
        ExprKind::Loop {
            vars,
            init,
            body: e1,
        } => {
            assert!(vars.len() == init.len());

            let body = &mut arena[bid].body;
            for i in 0..vars.len() {
                let v = vars[i].clone();
                let t = old_tyenv.get(&v).unwrap();

                tyenv.insert(v.clone(), t.clone());

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
            let loop_id = arena.alloc(mir::Block::with_name(id::gen_tmp_var_with(".loop")));

            arena[bid].tail.item = TailKind::Jump(loop_id);
            conv(
                e1,
                old_tyenv,
                tyenv,
                arena,
                loop_id,
                res,
                tail,
                Some(loop_id),
            );
        }
        ExprKind::Continue(xs) => {
            let body = &mut arena[bid].body;
            for (v, x) in xs {
                body.push((Some(v), InstKind::Mv(x).with_span(e.loc)));
            }

            arena[bid].tail = Box::new(TailKind::Jump(loop_id.unwrap()).with_span(e.loc));
        }
        ExprKind::CallDir(label, xs) => {
            if xs.len() > common::REGS.len() {
                panic!("too many arguments!!!");
            }

            let block = &mut arena[bid];
            for (x, r) in xs.into_iter().zip(common::REGS) {
                let reg = format!("%{}", r);
                let kind = match x {
                    virt::Value::Var(x) => InstKind::Mv(x),
                    virt::Value::Imm(x) => InstKind::Li(x as i32),
                };
                block.body.push((Some(reg), kind.with_span(e.loc)));
            }

            let reg = format!("%{}", common::REG_RET);
            block
                .body
                .push((Some(reg.clone()), InstKind::CallDir(label).with_span(e.loc)));
            block
                .body
                .push((Some(reg.clone()), InstKind::Mv(reg).with_span(e.loc)));
            block.tail = Box::new(tail.with_span(e.loc));
        }
        ExprKind::CallCls(..) => unimplemented!("I have no time"),
        _ => {
            let block = &mut arena[bid];
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

        let mut arena = id_arena::Arena::new();
        let entry_id = arena.alloc(mir::Block::with_name(format!(".Entry@{}", name)));

        // register fundef
        p.fundefs.push(mir::Fundef {
            name: Label(name),
            entry: entry_id,
            block_arena: arena,
        });

        let arena = &mut p.fundefs.last_mut().unwrap().block_arena;

        // load args
        if formal_fv.is_empty() {
            let block = &mut arena[entry_id];
            for (i, x) in args.into_iter().enumerate() {
                let r = format!("%{}", REGS[i]);
                block
                    .body
                    .push((Some(x), mir::InstKind::Mv(r).with_span(body.loc)));
            }
        } else {
            unimplemented!()
        }

        if let mir::Ty::Fun(_, rt) = ft {
            let is_unit = rt.as_ref() == &mir::Ty::Unit;
            if is_unit {
                conv(
                    body,
                    tyenv,
                    &mut p.tymap,
                    arena,
                    entry_id,
                    None,
                    mir::TailKind::Return,
                    None,
                );
            } else {
                let ret_var = format!("%{}", common::REG_RET);
                conv(
                    body,
                    tyenv,
                    &mut p.tymap,
                    arena,
                    entry_id,
                    Some(ret_var.clone()),
                    mir::TailKind::Return,
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
    {
        let body = &mut arena[exit].body;
        for _ in 0..10 {
            body.push((None, mir::InstKind::Nop.with_span((0, 0))));
        }
    }
    let mut p = mir::Program::new(arena, entry, exit);

    for (label, data) in prog.globals {
        let size = match data {
            virt::GlobalData::GInt16(_) => 1,
            virt::GlobalData::GInt32(_) => 1,
            virt::GlobalData::GFloat(_) => 1,
            virt::GlobalData::GSpace(s) => s,
        };
        p.globals.push((Label(label), size));
    }

    convert_fundef(prog.fundefs, &prog.tyenv, &mut p);

    conv(
        prog.main,
        &prog.tyenv,
        &mut p.tymap,
        &mut p.main_arena,
        entry,
        None,
        mir::TailKind::Jump(exit),
        None,
    );

    p
}
