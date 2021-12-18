mod compile;

use clap::Parser;
use compile::*;

use anyhow::Result;

fn main() -> Result<()> {
    simplelog::CombinedLogger::init(vec![
        // 標準出力にはWarn以上を表示する。
        simplelog::TermLogger::new(
            simplelog::LevelFilter::Warn,
            simplelog::Config::default(),
            simplelog::TerminalMode::Mixed,
            simplelog::ColorChoice::Auto
        ),
        // ファイルsimplelog.logにはInfo以上を表示する。
        // simplelog::WriteLogger::new(
        //     simplelog::LevelFilter::Info,
        //     simplelog::Config::default(),
        //     std::fs::File::create("simplelog.log").unwrap(),
        // ),
    ])
    .unwrap();

    let args = Args::parse();
    
    compile(args)
}
