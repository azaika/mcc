let a = Array.make 2 1 in
let b = Array.make 2 1 in
let rec f0 _ = (
  a.(0) <- 1;
  b.(1) <- a.(0)
) in
let rec f1 _ = (
  b.(1) <- 1;
  a.(0) <- b.(1)
) in
if read_int () = 0 then
  f0 ()
else
  f1 ();
print_int a.(0)