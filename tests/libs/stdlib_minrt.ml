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
let abs_float = fabs in
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
let rec print_newline _ =
  let x = 10 in (* '\n' = 10 *)
  _asmE"out" x;
  ()
in
let rec print_char x =
  let a = x * 1 in
  _asmE"out" x;
  ()
in
let rec print_2d x d =
  let dd = d * 10 in
  let r = x - dd in
  if r >= 0 then (
    print_char (d + 48);
    print_char (r + 48)
  ) else print_2d x (d - 1)
in
let rec print_byte x = (* assume `0 <= x < 256` *)
  let x = if x >= 500 then (
    print_char 53;
    x - 500
  ) else if x >= 400 then (
    print_char 52;
    x - 400
  ) else if x >= 300 then (
    print_char 51;
    x - 300
  ) else if x >= 200 then (
    print_char 50;
    x - 200
  ) else if x >= 100 then (
    print_char 49;
    x - 100
  ) else (
    print_char 48;
    x
  ) in
  (* x < 100 *)
  print_2d x 9
in
let print_int = print_byte in
let rec read_float _ =
  let a = _asmE"in" in
  let b = a *. 1.0 in
  a
in
let rec read_int _ =
  let a = _asmE"in" in
  let b = a * 1 in
  a
in
()