let rec sin x =
  let a = x *. 1.0 in
  let b = min_caml_sin x in
  let c = b *. 1.0 in
  b
in
let rec cos x =
  let a = x *. 1.0 in
  let b = min_caml_cos x in
  let c = b *. 1.0 in
  b
in
let rec sqrt x =
  let a = x *. 1.0 in
  let b = min_caml_sqrt x in
  let c = b *. 1.0 in
  b
in
let rec fabs x =
  let a = x *. 1.0 in
  let b = min_caml_fabs x in
  let c = b *. 1.0 in
  b
in
let rec atan x =
  let a = x *. 1.0 in
  let b = min_caml_atan x in
  let c = b *. 1.0 in
  b
in
let rec int_of_float x =
  let a = x *. 1.0 in
  let b = min_caml_int_of_float x in
  let c = b * 1 in
  b
in
let rec float_of_int x =
  let a = x * 1 in
  let b = min_caml_float_of_int x in
  let c = b *. 1.0 in
  b
in
let truncate = int_of_float
in
let rec floor x =
  let a = x *. 1.0 in
  let b = min_caml_floor x in
  let c = b *. 1.0 in
  b
in
let rec print_newline x =
  let a = x = () in
  min_caml_print_newline (); ()
in
let rec print_int x =
  let a = x * 1 in
  min_caml_print_int x; ()
in
let rec print_char x =
  let a = x * 1 in
  min_caml_print_byte x; ()
in
let rec read_float x =
  let a = x = () in
  let a = min_caml_read_float () in
  let b = a *. 1.0 in
  a
in
let rec read_int x =
  let a = x = () in
  let a = min_caml_read_int () in
  let b = a * 1 in
  a
in
()