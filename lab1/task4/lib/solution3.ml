open Common

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
  |> List.filter Common.is_palindrome
  |> List.map int_of_string
  |> max_element max
let _ = print_endline (string_of_int result)




