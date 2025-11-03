(* maximum *)
let max x y = if x > y then x else y

(* work with palindrom *)
let first_sign s = s.[0]
let last_sign s = s.[String.length s - 1] 
let get_middle s = String.sub s 1 (String.length s - 2) 

let rec is_palindrom s = if String.length s == 1 then true else
    match s with
    | "" -> true
    | _ -> (first_sign s == last_sign s) && is_palindrom (get_middle s)

(* with recursion *)
let rec f2 x y = match x with
    | 99 -> 0
    | _ -> let z = x * y in if is_palindrom(string_of_int z) then max z (f2 (x-1) y) else f2 (x-1) y

let rec f1 x = match x with
    | 99 -> 0
    | _ -> let y = f2 999 x in if is_palindrom(string_of_int y) then max y (f1 (x-1) ) else f1 (x-1)

let _ = print_endline (string_of_int (f1 999))

(* with tail recursion*)
let rec f2_tr x y acc = match x with
    | 99 -> acc
    | _ -> let z = x * y in if is_palindrom(string_of_int z) then f2_tr (x - 1) y (max z acc) else f2_tr (x-1) y acc

let rec f1_tr x acc = match x with
    | 99 -> acc
    | _ -> let y = f2_tr 999 x 0 in if is_palindrom(string_of_int y) then f1_tr (x-1) (max y acc) else f1_tr (x-1) acc

let _ = print_endline (string_of_int (f1_tr 999 0))

(* with modular style *)
let rec ( -- ) (i1, j1) (i2, j2) = 
  if i1 > i2 || j1 > j2 then []
  else if j1 == j2 then (i1, j1) :: (i1 + 1, 100) -- (i2, j2)
  else (i1, j1) :: (i1, j1 + 1) -- (i2, j2)
let get_multiple (x, y) = string_of_int (x * y)
let max_element compare lst = 
  match lst with 
  | [] -> 0
  | h :: t -> List.fold_left compare h t

let result =
  (100, 100) -- (999, 999)
  |> List.map get_multiple
  |> List.filter is_palindrom
  |> List.map int_of_string
  |> max_element max
let _ = print_endline (string_of_int result)

(* with circles *)
let find_answer_with_circles =
  let acc = ref 0 in
  for a = 100 to 999 do 
    for b = 100 to 999 do 
      if is_palindrom (string_of_int (a * b)) then acc := max !acc (a * b)
    done
  done;
  !acc
let _ = print_endline (string_of_int find_answer_with_circles)

(* with sequence *)
type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t
let rec map_lazy f (Cons (h, t)) = Cons(f h, lazy(map_lazy f (Lazy.force t)))

let rec palindrom = Cons((100, 100), lazy (map_lazy (fun (x, y) -> if y == 999 then (x+1, 100) else (x, y+1)) palindrom))

let rec find_answer_lazy (Cons((x, y), t)) = if x == 999 then 0 else (if is_palindrom (string_of_int (x * y)) then max (x * y) (find_answer_lazy (Lazy.force t)) else (find_answer_lazy (Lazy.force t)))

let _ = print_endline (string_of_int (find_answer_lazy palindrom))
