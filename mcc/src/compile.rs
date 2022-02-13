use std::io::Write;
use std::path::Path;

use anyhow::{Context, Result};
use clap::Parser;

use ariadne;
use ast::{knormal, syntax};

#[derive(Debug, Parser)]
pub struct Args {
    #[clap(long, default_value_t = 100)]
    pub inline: usize,
    #[clap(long, default_value_t = 100)]
    pub loop_opt: usize,
    #[clap(long)]
    pub use_strict_aliasing: bool,
    #[clap(short, long)]
    pub optimize: bool,
    #[clap(short, long)]
    pub verbose: bool,
    #[clap(short, long)]
    pub debug: bool,
    #[clap(short, long)]
    pub lib: Option<Vec<String>>,
    pub source: String,
}

fn parse_file(path: &str) -> Result<syntax::Expr> {
    let src = std::fs::read_to_string(path).context(format!("failed to open file: {}", path))?;

    parser::parse(&src).map_err(|err| {
        use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind, Source};

        let mut colors = ColorGenerator::new();

        // Generate & choose some colours for each of our elements
        let a = colors.next();

        Report::build(ReportKind::Error, path, err.loc.0)
            .with_code(3)
            .with_message(err.item.to_string())
            .with_label(
                Label::new((path, err.into()))
                    .with_message(format!("error found {}", "here".fg(a)))
                    .with_color(a),
            )
            .finish()
            .print((path, Source::from(src)))
            .unwrap();

        anyhow::Error::msg("aborting due to the error above")
    })
}

fn infer(e: syntax::Expr, path: &str) -> Result<(syntax::Expr, typing::TypeMap)> {
    typing::infer(e).map_err(|err| {
        use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind, Source};

        let src = std::fs::read_to_string(path)
            .context(format!("failed to open file: {}", path))
            .unwrap();

        let mut colors = ColorGenerator::new();

        // Generate & choose some colors for each of our elements
        let a = colors.next();

        Report::build(ReportKind::Error, path, err.span.0)
            .with_code(3)
            .with_message(err.to_string())
            .with_label(
                Label::new((path, err.span.0..err.span.1))
                    .with_message(format!("error found {}", "here".fg(a)))
                    .with_color(a),
            )
            .finish()
            .print((path, Source::from(src)))
            .unwrap();

        anyhow::Error::msg("aborting due to the error above")
    })
}

fn optimize_knorm(mut e: knormal::Expr, tyenv: &mut knorm::TyMap, config: &Args) -> knormal::Expr {
    let mut prev = e.clone();
    // ループの変換は一回だけやる (多重再帰のループ性判定はしない)
    e = knorm::flatten_let(e);
    e = knorm::detect_loop(e, tyenv);
    e = knorm::simplify_loop(e, tyenv);

    for i in 0..config.loop_opt {
        log::info!("knorm opt loop: {}", i + 1);
        e = knorm::eliminate(e, tyenv);
        e = knorm::flatten_let(e);
        e = knorm::fold_const(e, tyenv);
        e = knorm::compress_onehot_if(e);
        e = knorm::beta_reduction(e, tyenv);
        e = knorm::eliminate(e, tyenv);
        e = knorm::cse(e, tyenv);
        e = knorm::beta_reduction(e, tyenv);
        e = knorm::eliminate(e, tyenv);
        e = knorm::inlining(e, config.inline, tyenv);
        e = knorm::flatten_let(e);
        e = knorm::compress_onehot_if(e);
        e = knorm::fold_const(e, tyenv);
        e = knorm::beta_reduction(e, tyenv);
        e = knorm::eliminate(e, tyenv);

        if e == prev {
            break;
        }
        prev = e.clone();
    }

    e
}

fn optimize_closure(mut p: ast::closure::Program, option: &Args) -> ast::closure::Program {
    let mut prev = p.clone();

    p = cls::detect_doall(p);
    for i in 0..100 {
        log::info!("closure opt loop: {}", i + 1);
        p = cls::fold_const(p);
        p = cls::flatten(p, option.use_strict_aliasing);
        p = cls::eliminate_get(p, option.use_strict_aliasing);
        p = cls::beta_reduction(p);
        p = cls::eliminate_var(p);

        if p == prev {
            break;
        }
        prev = p.clone();
    }

    p
}

// fn optimize_mir(mut p: ast::mir::Program) -> ast::mir::Program {
//     let mut prev = p.clone();
//     for i in 0..100 {
//         log::info!("mir opt loop: {i}");
//         p = cfg::skip_jump(p);

//         if p == prev {
//             break;
//         }
//         prev = p.clone();
//     }

//     p
// }

fn debug_output(path: &Path, s: String) -> Result<()> {
    if !Path::new(&"debug").exists() {
        std::fs::create_dir("debug")?;
    }
    let path = Path::new("debug").join(path);
    std::fs::File::create(path)
        .context("failed to open debug file")?
        .write_all(s.as_bytes())
        .context("failed to write debug infomation")
}

pub fn compile(args: Args) -> Result<()> {
    let parsed_libs = match &args.lib {
        None => vec![],
        Some(libs) => libs
            .iter()
            .map(|x| parse_file(x))
            .collect::<Result<Vec<syntax::Expr>>>()?,
    };
    let parsed_src = parse_file(&args.source)?;

    // ライブラリを連結
    let parsed = if let Some(libs) = parsed_libs.into_iter().reduce(|x, y| *syntax::concat(x, y)) {
        *syntax::concat(libs, parsed_src)
    } else {
        parsed_src
    };

    let (typed, extenv) = infer(parsed, &args.source)?;

    if args.verbose {
        debug_output(Path::new("typed.txt"), format!("[[typed]]\n{}", typed))?;
    }

    let knormed = knorm::convert(typed, &extenv)?;

    if args.verbose {
        debug_output(
            Path::new("knormed.txt"),
            format!("[[knormed]]\n{}", knormed),
        )?;
    }

    let (alpha, mut tyenv) = knorm::to_alpha_form(knormed);

    if args.verbose {
        debug_output(Path::new("alpha.txt"), format!("[[alpha]]\n{}", alpha))?;
    }

    let opt_knorm = if args.optimize {
        let r = optimize_knorm(alpha, &mut tyenv, &args);

        if args.verbose {
            debug_output(
                Path::new("knorm_opt.txt"),
                format!("[[optimized_knorm]]\n{}", r),
            )?;
        }

        r
    } else {
        knorm::flatten_let(alpha)
    };

    let closured = cls::convert(opt_knorm, tyenv);
    if args.verbose {
        debug_output(
            Path::new("closure.txt"),
            format!("[[closured]]\n{}", closured),
        )?;
    }

    let opt_closure = if args.optimize {
        let r = optimize_closure(closured, &args);

        if args.verbose {
            debug_output(
                Path::new("closure_opt.txt"),
                format!("[[optimized_closure]]\n{}", r),
            )?;
        }

        r
    } else {
        closured
    };

    let consts = cls::collect_consts(&opt_closure);
    let virt = arch::to_virtual(opt_closure, &consts);
    if args.verbose {
        debug_output(Path::new("virtual.txt"), format!("[[virtual]]\n{}", virt))?;
    }

    let opt_virt = if args.optimize {
        let virt = arch::optimize_virtual(virt);
        arch::finalize_virt(virt)
    } else {
        arch::finalize_virt(virt)
    };
    if args.verbose {
        debug_output(
            Path::new("virtual_opt.txt"),
            format!("[[virtual_opt]]\n{}", opt_virt),
        )?;
    }

    let mir = arch::to_mir(opt_virt);
    if args.verbose {
        debug_output(Path::new("mir.txt"), format!("[[mir]]\n{mir}"))?;
    }
    let mir_opt = if args.optimize {
        arch::optimize_mir(mir)
    } else {
        mir
    };
    if args.verbose {
        debug_output(
            Path::new("mir_opt.txt"),
            format!("[[mir_opt]]\n{}", mir_opt),
        )?;
    }

    let (regalloc, m1, m2) = arch::do_regalloc(mir_opt);
    if args.verbose {
        debug_output(
            Path::new("regalloc.txt"),
            format!("[[regalloc]]\n{}", regalloc),
        )?;
    }

    let mut file = std::fs::File::create(Path::new("out.a")).context("failed to open debug file")?;
    arch::emit(&mut file, regalloc, (m1, m2))?;

    Ok(())
}
