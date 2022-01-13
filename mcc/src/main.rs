mod compile;

use clap::Parser;
use compile::*;

use anyhow::Result;

fn main() -> Result<()> {
    let args = Args::parse();

    if args.debug {
        simplelog::CombinedLogger::init(vec![
            // 標準出力には Debug 以上を表示する。
            simplelog::TermLogger::new(
                simplelog::LevelFilter::Debug,
                simplelog::Config::default(),
                simplelog::TerminalMode::Mixed,
                simplelog::ColorChoice::Auto,
            ),
            // ファイルsimplelog.logにはInfo以上を表示する。
            // simplelog::WriteLogger::new(
            //     simplelog::LevelFilter::Info,
            //     simplelog::Config::default(),
            //     std::fs::File::create("simplelog.log").unwrap(),
            // ),
        ])
        .unwrap();
    } else {
        simplelog::CombinedLogger::init(vec![
            // 標準出力にはWarn以上を表示する。
            simplelog::TermLogger::new(
                simplelog::LevelFilter::Info,
                simplelog::Config::default(),
                simplelog::TerminalMode::Mixed,
                simplelog::ColorChoice::Auto,
            ),
        ])
        .unwrap();
    }

    compile(args)
}
