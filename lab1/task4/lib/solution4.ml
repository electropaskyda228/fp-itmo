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