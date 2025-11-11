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