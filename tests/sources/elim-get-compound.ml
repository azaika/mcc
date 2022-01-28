let aa = Array.make 2 (Array.make 0 0) in
let bb = Array.make 1 (Array.make 0 1) in
let a = Array.make 5 1 in
let x = a.(0) in
aa.(0) <- a;
aa.(1) <- aa.(0);
bb.(0) <- (Array.make 1 3);
aa.(1).(0) <- 2;
let y = a.(0) in
let z = bb.(0).(0) in
dummy(x);
dummy(y);
dummy(z)