use anyhow::{Result, Context};
use clap::Parser;

use ast::*;
use ariadne;

#[derive(Debug, Parser)]
pub struct Args {
    #[clap(long, default_value_t=200)]
    pub inline: usize,
    #[clap(long, default_value_t=100)]
    pub loop_opt: usize,
    #[clap(short, long)]
    pub optimize: bool,
    #[clap(short, long)]
    pub verbose: bool,
    #[clap(short, long)]
    pub lib: Option<Vec<String>>,
    pub source: String
}

fn parse_file(path: &str) -> Result<syntax::Expr> {
    let src = std::fs::read_to_string(path).context(format!("failed to open file: {}", path))?;

    parser::parse(&src).map_err(|err| {
        use ariadne::{Report, ReportKind, Label, Source, ColorGenerator, Fmt};

        let mut colors = ColorGenerator::new();

        // Generate & choose some colours for each of our elements
        let a = colors.next();

        Report::build(ReportKind::Error, path, err.loc.0)
            .with_code(3)
            .with_message(err.item.to_string())
            .with_label(Label::new((path, err.into()))
                .with_message(format!("error found {}", "here".fg(a)))
                .with_color(a))
            .finish()
            .print((path, Source::from(src)))
            .unwrap();
        
        anyhow::Error::msg("aborting due to the error above")
    })
}

fn infer(e: syntax::Expr, path: &str) -> Result<(syntax::Expr, typing::TypeMap)> {
    typing::infer(e).map_err(|err|{
        use ariadne::{Report, ReportKind, Label, Source, ColorGenerator, Fmt};

        let src = std::fs::read_to_string(path).context(format!("failed to open file: {}", path)).unwrap();

        let mut colors = ColorGenerator::new();

        // Generate & choose some colours for each of our elements
        let a = colors.next();

        Report::build(ReportKind::Error, path, err.span.0)
            .with_code(3)
            .with_message(err.to_string())
            .with_label(Label::new((path, err.span.0 .. err.span.1))
                .with_message(format!("error found {}", "here".fg(a)))
                .with_color(a))
            .finish()
            .print((path, Source::from(src)))
            .unwrap();
        
        anyhow::Error::msg("aborting due to the error above")
    })
}

fn optimize_knorm(mut e: knormal::Expr, tyenv: &mut knorm::TyMap, config: &Args) -> knormal::Expr {
    let mut prev = e.clone();
    for _ in 0..config.loop_opt {
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

pub fn compile(args : Args) -> Result<()> {
    let parsed_libs = match &args.lib {
        None => vec![],
        Some(libs) => {
            libs.iter().map(|x| parse_file(x)).collect::<Result<Vec<syntax::Expr>>>()?
        }
    };
    let parsed_src = parse_file(&args.source)?;

    // ライブラリを連結
    let parsed = if let Some(libs) = parsed_libs.into_iter().reduce(|x, y| *syntax::concat(x, y)) {
        *syntax::concat(libs,parsed_src)
    }
    else {
        parsed_src
    };

    let (typed, extenv) = infer(parsed, &args.source)?;

    if args.verbose {
        println!("[[typed]]\n{}", typed);
    }

    let knormed = knorm::convert(typed, &extenv)?;

    if args.verbose {
        println!("[[knormed]]\n{}", knormed);
    }

    let (alpha, mut tyenv) = knorm::to_alpha_form(knormed);

    if args.verbose {
        println!("[[alpha]]\n{}", alpha);
    }

    let _opt_knorm = if args.optimize {
        let r = optimize_knorm(alpha, &mut tyenv, &args);

        if args.verbose {
            println!("[[optimized_knorm]]\n{}", r);
        }
        
        r
    }
    else {
        alpha
    };

    Ok(())
}
