open Common

(* with tail recursion*)
let rec f1_tr x y acc =
  if x < 100 then acc
  else if y < 100 then f1_tr (x - 1) 999 acc
  else
    let product = x * y in
    let new_acc = 
      if Common.is_palindrome(product) 
      then Common.max product acc 
      else acc
    in
    f1_tr x (y - 1) new_acc

let result = f1_tr 999 999 0

let _ = print_endline (string_of_int (f1_tr 999 999 0))