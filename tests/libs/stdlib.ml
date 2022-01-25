let rec sin x =
  let a = x *. 1.0 in
  let b = _asm"fsin" x in
  let c = b *. 1.0 in
  b
in
let rec cos x =
  let a = x *. 1.0 in
  let b = _asm"fcos" x in
  let c = b *. 1.0 in
  b
in
let rec sqrt x =
  let a = x *. 1.0 in
  let b = _asm"fsqrt" x in
  let c = b *. 1.0 in
  b
in
let rec fabs x =
  let a = x *. 1.0 in
  let b = _asm"fabs" x in
  let c = b *. 1.0 in
  b
in
let rec atan x =
  let a = x *. 1.0 in
  let b = _asm"fatan" x in
  let c = b *. 1.0 in
  b
in
let rec int_of_float x =
  let a = x *. 1.0 in
  let b = _asm"ftoi" x in
  let c = b * 1 in
  b
in
let rec float_of_int x =
  let a = x * 1 in
  let b = _asm"itof" x in
  let c = b *. 1.0 in
  b
in
let truncate = int_of_float
in
let rec floor x =
  let a = x *. 1.0 in
  let b = _asm"ffloor" x in
  let c = b *. 1.0 in
  b
in
let rec print_newline x =
  x;
  let x = 10 in (* '\n' = 10 *)
  _asmE"out" x; ()
in
let rec print_int x =
  let a = x * 1 in
  min_caml_print_int x; ()
in
let rec print_char x =
  let a = x * 1 in
  _asmE"out" x; ()
in
let rec read_float x =
  x;
  let a = _asmE"in" in
  let b = a *. 1.0 in
  a
in
let rec read_int x =
  x;
  let a = _asmE"in" in
  let b = a * 1 in
  a
in
()