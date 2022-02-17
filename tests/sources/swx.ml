let a = Array.make 2 0 in
let rec one _ = 1 in
let rec f x n = (
    a.(x) <- n
) in
f (one ()) 1;
f (one ()) 2;
print_int a.(0);
print_int a.(1)