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

(* with map generate *)
let rec ( -- ) (i1, j1) (i2, j2) = 
  if i1 > i2 || j1 > j2 then [] 
  else if j1 == j2 then (i1, j1) :: (i1 + 1, -1000) -- (i2, j2)
  else (i1, j1) :: (i1, j1 + 1) -- (i2, j2)
let get_info (a, b) = ((sort_through_n 0 a b), a * b)
let max_element compare lst = 
  match lst with 
  | [] -> (0, 0)
  | h :: t -> List.fold_left compare h t 

let result = 
  (-1000, -1000) -- (1000, 1000)
  |> List.map get_info
  |> max_element max
let _ = print_endline (string_of_int (fst result))
let _ = print_endline (string_of_int (snd result))