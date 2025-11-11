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

(* with tail recursion*)
let rec f2_tr x y acc = match x with
    | 99 -> acc
    | _ -> let z = x * y in if is_palindrom(string_of_int z) then f2_tr (x - 1) y (max z acc) else f2_tr (x-1) y acc

let rec f1_tr x acc = match x with
    | 99 -> acc
    | _ -> let y = f2_tr 999 x 0 in if is_palindrom(string_of_int y) then f1_tr (x-1) (max y acc) else f1_tr (x-1) acc

let _ = print_endline (string_of_int (f1_tr 999 0))