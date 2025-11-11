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

(* with map generate *)
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




