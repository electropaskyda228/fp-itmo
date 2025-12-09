open Common

(* with resursion *)
let find_answer_with_recursion a_max b_max = 
  let rec sort_through_a a = 
    match a with
    | _ when a = -a_max - 1 -> (0, 0)
    | _ -> let rec sort_through_b b = 
      match b with 
      | _ when b = -b_max - 1 -> (0, 0)
      | _ -> max (Common.sort_through_n 0 a b, a * b) (sort_through_b (b - 1))
      in max (sort_through_b b_max) (sort_through_a (a - 1)) in
  sort_through_a a_max

let (first, second) = find_answer_with_recursion 1000 1000
let _ = print_endline (string_of_int(first))
let _ = print_endline (string_of_int(second))