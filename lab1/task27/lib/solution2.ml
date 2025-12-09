open Common

(* with tail recursion *)
let find_answer_with_tail_recursion a_max b_max = 
  let rec sort_through_a a acc = 
    match a with
    | _ when a = -a_max - 1 -> acc
    | _ -> let rec sort_through_b b acc = 
      match b with 
      | _ when b = -b_max - 1 -> acc
      | _ -> sort_through_b (b - 1) (max (Common.sort_through_n 0 a b, a * b) acc)
      in sort_through_a (a - 1) (max (sort_through_b b_max (0, 0)) acc) in
  sort_through_a a_max (0, 0)

let (fisrt_s, second_s) = find_answer_with_tail_recursion 1000 1000
let _ = print_endline (string_of_int(fisrt_s))
let _ = print_endline (string_of_int(second_s))