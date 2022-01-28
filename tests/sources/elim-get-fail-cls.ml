let one = 1 in
let a = Array.make 2 0 in
let b = Array.make 2 0 in
let rec dummy _ = () in
let cls = Array.make 1 dummy in
let rec set_cls f =
  cls.(0) <- f
in
let rec f0 _ = (
  a.(0) <- 1;
  b.(1) <- a.(0);
  set_cls dummy;
  cls.(0) ()
) in
let rec f1 _ = (
  b.(1) <- 1;
  set_cls dummy;
  a.(0) <- b.(1)
) in
if read_int () = 0 then
  f0 ()
else
  f1 ();
print_int one;
print_int a.(0)