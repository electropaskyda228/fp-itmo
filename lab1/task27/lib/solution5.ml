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

(* with sequence*)
type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t
let rec map f (Cons(h, t)) = Cons(f h, lazy(map f (Lazy.force t)))
let rec sort_through = Cons((-1000, -1000), 
  lazy(map (fun (x, y) -> if y == 1000 then (x + 1, -1000) else (x, y + 1)) sort_through))

let rec find_answer_with_sequence (Cons((a, b), t)) = 
  if a == 1001 then (0, 0) else max ((sort_through_n 0 a b), a * b) (find_answer_with_sequence (Lazy.force t))

let (first_s, second_s) = find_answer_with_sequence sort_through
let _ = print_endline (string_of_int (first_s))
let _ = print_endline(string_of_int (second_s))