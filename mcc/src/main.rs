use anyhow::{Result, Context};
use clap::Parser;

use ast::syntax;
use ariadne;

#[derive(Debug, Parser)]
struct Args {
    #[clap(long, default_value_t=200)]
    inline: i32,
    #[clap(long, default_value_t=100)]
    loop_opt: i32,
    #[clap(short, long)]
    optimize: bool,
    #[clap(short, long)]
    verbose: bool,
    #[clap(short, long)]
    lib: Option<Vec<String>>,
    source: String
}

fn parse_file(path: &str) -> Result<syntax::Expr> {
    let src = std::fs::read_to_string(path).context(format!("failed to open file: {}", path))?;

    parser::parse(&src).map_err(|err| {
        use ariadne::{Report, ReportKind, Label, Source, ColorGenerator, Fmt};

        let mut colors = ColorGenerator::new();

        // Generate & choose some colours for each of our elements
        let a = colors.next();

        Report::build(ReportKind::Error, path, 12)
            .with_code(3)
            .with_message(err.item.to_string())
            .with_label(Label::new((path, err.into()))
                .with_message(format!("error found {}", "here".fg(a)))
                .with_color(a))
            .finish()
            .print((path, Source::from(src)))
            .unwrap();
        
        anyhow::Error::msg("aborting because of the error above")
    })
}

fn compile(args : Args) -> Result<()> {
    let parsed_libs = match args.lib {
        None => vec![],
        Some(libs) => {
            libs.iter().map(|x| parse_file(&x)).collect::<Result<Vec<syntax::Expr>>>()?
        }
    };
    let parsed_src = parse_file(&args.source)?;

    println!("{:?}", parsed_libs);
    println!("{}", parsed_src);
    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();
    
    compile(args)
}
