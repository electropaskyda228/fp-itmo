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

(* with circles *)
let find_answer_with_circles =
  let r = ref (0, 0) in
  for a = -1000 to 1000 do
    for b = -1000 to 1000 do
      let n = ref 0 in
      while is_prime (f !n a b) do
        n := !n + 1 
      done;
      if fst !r < !n then r := (!n - 1, a * b)
    done
  done;
  fst !r
let _ = print_endline (string_of_int (find_answer_with_circles))