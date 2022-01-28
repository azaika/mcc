let a = read_int () in
let rec is_zero x = x = 0 in
let x = if is_zero a then 0 else 1 in
print_int x