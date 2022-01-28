let rec f a b =
  a.(0) <- 1;
  b.(0) <- 3;
  a.(0)
in
dummy (f (Array.make 1 1) (Array.make 1 1));
dummy (f (Array.make 1 1) (Array.make 1 1))