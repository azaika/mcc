use mcc::*;

use std::fs;

fn read_dir(path: &str) -> Vec<String> {
    let dir = fs::read_dir(path).unwrap();
    dir.into_iter()
        .map(|x| x.unwrap().path().into_os_string().into_string().unwrap())
        .collect()
}

fn dummy_arg(path: &str) -> Args {
    Args {
        inline: 100,
        loop_opt: 200,
        optimize: true,
        use_strict_aliasing: false,
        verbose: false,
        debug: false,
        lib: Some(vec!["../tests/libs/stdlib.ml".to_string()]),
        source: path.to_string(),
    }
}

fn dummy_arg_minrt(path: &str) -> Args {
    Args {
        inline: 100,
        loop_opt: 200,
        optimize: true,
        use_strict_aliasing: true,
        verbose: false,
        debug: false,
        lib: Some(vec![
            "../tests/minrt/globals.ml".to_string(),
            "../tests/libs/stdlib_minrt.ml".to_string(),
        ]),
        source: path.to_string(),
    }
}

#[test]
fn test_minrt() {
    let src = "../tests/minrt/minrt.ml".to_string();
    let arg = dummy_arg_minrt(&src);

    assert!(compile(arg).is_ok());
}

#[test]
fn test_all() {
    let srcs = read_dir("../tests/sources/");
    let args: Vec<_> = srcs.into_iter().map(|x| dummy_arg(&x)).collect();

    for arg in args {
        println!("testing {}", arg.source);
        assert!(compile(arg).is_ok());
    }
}
