let cls = Array.make 1 (let rec dummy x = () in dummy) in
let rec f a =
  let rec g y = (a.(0) <- y) in
  cls.(0) <- g
in
let a = Array.make 1 2 in
let b = Array.make 1 3 in
f a;
print_int a.(0); (* 2 *)
(cls.(0)) 4;
print_int a.(0); (* 4 *)
f b;
print_int b.(0); (* 3 *)
(cls.(0)) 5;
print_int b.(0) (* 5 *)