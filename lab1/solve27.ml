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

(* with resursion *)
let find_answer_with_recursion a_max b_max = 
  let rec sort_through_a a = 
    match a with
    | _ when a = -a_max - 1 -> (0, 0)
    | _ -> let rec sort_through_b b = 
      match b with 
      | _ when b = -b_max - 1 -> (0, 0)
      | _ -> max (sort_through_n 0 a b, a * b) (sort_through_b (b - 1))
      in max (sort_through_b b_max) (sort_through_a (a - 1)) in
  sort_through_a a_max

let (first, second) = find_answer_with_recursion 1000 1000
let _ = print_endline (string_of_int(first))
let _ = print_endline (string_of_int(second))

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

let (fisrt, second) = find_answer_with_tail_recursion 1000 1000
let _ = print_endline (string_of_int(first))
let _ = print_endline (string_of_int(second))

(* with modular style *)
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


