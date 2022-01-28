let rec set index aa bb cc =
  if index < 3 then (
    let tmp = aa.(index) in
    aa.(index) <- bb.(index);
    bb.(index) <- cc.(index);
    cc.(index) <- tmp;
    set (index + 1) bb cc aa
  ) else ()
in
let a = Array.make 3 1 in
let b = Array.make 3 2 in
let c = Array.make 3 3 in
set 0 a b c