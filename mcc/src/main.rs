mod compile;

use clap::Parser;
use compile::*;

use anyhow::Result;

fn main() -> Result<()> {
    let args = Args::parse();
    
    compile(args)
}
