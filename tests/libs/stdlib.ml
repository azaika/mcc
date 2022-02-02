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
let rec rem x a d =
  if x >= a then rem (x - a) a (d + 1) else (d, x)
in
let rec print_byte x = (* assume `x < 256` *)
  let x = if x >= 200 then (
    print_char 50;
    x - 200
  ) else if x >= 100 then (
    print_char 49;
    x - 100
  ) else
    x
  in
  (* x < 100 *)
  if x < 10 then
    print_char (x + 48)
  else
    let (d, r) = rem x 10 0 in
    if d = 0 then
      print_char (r + 48)
    else (
      print_char (d + 48);
      print_char (r + 48)
    )
in
let rec print_int_final a index emerged =
  if index >= 0 then (
    let d = a.(index) in
    if d = 0 then (
      if emerged then print_char 48 else ();
      print_int_final a (index - 1) emerged
    )
    else (
      print_char (d + 48);
      print_int_final a (index - 1) true
    )
  )
  else ()
in
let rec print_int x =
  let a = x * 1 in
  let x = if x < 0
    then (
      print_char 45; (* '-' = 45 *)
      -x
    )
    else
      x
  in
  (* x >= 0 *)
  if x = 0 then print_char 48
  else if x < 256 then print_byte x
  else (
    let res = Array.make 10 0 in
    let x = if x >= 1000000000 then (
      let (d, r) = rem x 1000000000 0 in
      res.(9) <- d;
      r
    ) else x in
    let x = if x >= 100000000 then (
      let (d, r) = rem x 100000000 0 in
      res.(8) <- d;
      r
    ) else x in
    let x = if x >= 10000000 then (
      let (d, r) = rem x 10000000 0 in
      res.(7) <- d;
      r
    ) else x in
    let x = if x >= 1000000 then (
      let (d, r) = rem x 1000000 0 in
      res.(6) <- d;
      r
    ) else x in
    let x = if x >= 100000 then (
      let (d, r) = rem x 100000 0 in
      res.(5) <- d;
      r
    ) else x in
    let x = if x >= 10000 then (
      let (d, r) = rem x 10000 0 in
      res.(4) <- d;
      r
    ) else x in
    let x = if x >= 1000 then (
      let (d, r) = rem x 1000 0 in
      res.(3) <- d;
      r
    ) else x in
    let x = if x >= 100 then (
      let (d, r) = rem x 100 0 in
      res.(2) <- d;
      r
    ) else x in
    let x = if x >= 10 then (
      let (d, r) = rem x 10 0 in
      res.(1) <- d;
      r
    ) else x in
    res.(0) <- x;
    print_int_final res 9 false
  )
in
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