(* common *)
let f n a b = n * n + a * n + b 

let max (n1, p1) (n2, p2) = 
  if n1 >= n2 then (n1, p1) else (n2, p2)

(* check whether number is prime *)
let is_prime n = 
  if n < 2 then false
  else 
    let rec check p = 
      if p * p > n then true
      else if n mod p == 0 then false
      else check (p+1)
    in check 2

let rec sort_through_n n a b= 
  if is_prime (f n a b) then sort_through_n (n + 1) a b else (n - 1)

(* with tail recursion *)
let find_answer_with_tail_recursion a_max b_max = 
  let rec sort_through_a a acc = 
    match a with
    | _ when a = -a_max - 1 -> acc
    | _ -> let rec sort_through_b b acc = 
      match b with 
      | _ when b = -b_max - 1 -> acc
      | _ -> sort_through_b (b - 1) (max (sort_through_n 0 a b, a * b) acc)
      in sort_through_a (a - 1) (max (sort_through_b b_max (0, 0)) acc) in
  sort_through_a a_max (0, 0)

let (fisrt_s, second_s) = find_answer_with_tail_recursion 1000 1000
let _ = print_endline (string_of_int(fisrt_s))
let _ = print_endline (string_of_int(second_s))