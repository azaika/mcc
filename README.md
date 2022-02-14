# MCC
min-caml language compiler written in rust.

This compiler is part of the submittion of cpu-ex 2021 team 5 and only supports our original ISA backend.

# Run

## Install rust
see [Install Rust](https://www.rust-lang.org/tools/install)

## run compiler
```sh
git clone https://github.com/azaika/mcc
cd mcc
rustup install nightly # mcc uses nightly-rust
rustup override add nightly
# compiles raytracing and emits debug outputs in debug/
cargo run --release -- -l ./tests/minrt/globals.ml -l ./tests/libs/stdlib_minrt.ml ./tests/minrt/minrt.ml -v -o -d --inline 1000 --use-strict-aliasing > debug/out.txt
```

## Known Bug
- When comments include multi-byte strings, this compiler emits wrong locations in the source file on compile error
