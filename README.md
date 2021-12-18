# MCC
min-caml language compiler written in rust.

This compiler is part of the submittion of cpu-ex 2021 team 5 and only supports our original ISA backend.

# Run

```sh
git clone https://github.com/azaika/mcc
cd mcc
cargo run
```

## Known Bug
- When comments include multi-byte strings, compiler emits wrong report about location in source file 