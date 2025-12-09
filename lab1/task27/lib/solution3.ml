open Common

(* with map generate *)
let rec ( -- ) (i1, j1) (i2, j2) = 
  if i1 > i2 || j1 > j2 then [] 
  else if j1 == j2 then (i1, j1) :: (i1 + 1, -1000) -- (i2, j2)
  else (i1, j1) :: (i1, j1 + 1) -- (i2, j2)
let get_info (a, b) = ((Common.sort_through_n 0 a b), a * b)
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