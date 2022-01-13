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

    for i in 0..config.loop_opt {
        log::info!("knorm opt loop: {}", i);
        e = knorm::eliminate(e);
        e = knorm::flatten_let(e);
        e = knorm::fold_const(e, tyenv);
        e = knorm::beta_reduction(e, tyenv);
        e = knorm::eliminate(e);
        e = knorm::cse(e, tyenv);
        e = knorm::beta_reduction(e, tyenv);
        e = knorm::eliminate(e);
        e = knorm::inlining(e, config.inline, tyenv);
        e = knorm::flatten_let(e);
        e = knorm::fold_const(e, tyenv);
        e = knorm::beta_reduction(e, tyenv);
        e = knorm::eliminate(e);

        if e == prev {
            break;
        }
        prev = e.clone();
    }

    e
}

fn optimize_mir(mut p: ast::mir::Program) -> ast::mir::Program {
    let mut prev = p.clone();
    for i in 0..100 {
        log::info!("mir opt loop: {i}");
        p = cfg::compress_jump(p);

        if p == prev {
            break;
        }
        prev = p.clone();
    }

    p
}

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

        r
    } else {
        knorm::flatten_let(alpha)
    };

    if args.verbose {
        debug_output(
            Path::new("opt_knorm.txt"),
            format!("[[optimized_knorm]]\n{}", opt_knorm),
        )?;
    }

    let closured = cls::convert(opt_knorm, tyenv);
    if args.verbose {
        debug_output(
            Path::new("closure.txt"),
            format!("[[closured]]\n{}", closured),
        )?;
    }

    let mir = cfg::convert(closured);
    if args.verbose {
        debug_output(Path::new("mir.txt"), format!("[[mir]]\n{}", mir))?;
    }

    let _opt_mir = if args.optimize {
        let r = optimize_mir(mir);

        if args.verbose {
            debug_output(
                Path::new("opt_mir.txt"),
                format!("[[optimized_mir]]\n{}", r),
            )?;
        }

        r
    } else {
        mir
    };

    Ok(())
}
