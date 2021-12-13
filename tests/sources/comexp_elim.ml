let a = 3 in
let b = 4 in
let x = (let s = a + b - 1 in s + s) in
let y = (let s = b + a - 1 in s + s) in
let z = (let s = a + b - 1 in s + s) in
let w = (let t = a + b - 1 in t + t) in
let w = y + z + w in
let x = x + x + x in
print_int (x + w)