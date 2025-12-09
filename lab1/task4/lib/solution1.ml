open Common

(* with recursion *)
let rec f1 x best =
  let rec f2 y current_best =
    match y with
    | 99 -> current_best
    | _ ->
        let product = x * y in
        let new_best = 
          if Common.is_palindrome product && product > current_best 
          then product 
          else current_best
        in
        f2 (y - 1) new_best
  in
  match x with
  | 99 -> best
  | _ ->
      let inner_result = f2 999 best in
      f1 (x - 1) inner_result

let result = f1 999 0

let () = 
  print_endline (string_of_int (f1 999 0))