let dummy = (12, 34, 2.0) in
let a = Array.make 5 dummy in
let rec f index =
  let (x, y, z) = a.(index) in
  x - y + (int_of_float z)
in
let update = (56, 78, 4.0) in
a.(3) <- update;
print_int (f 1);
print_int (f 3)