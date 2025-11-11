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

(* with sequence *)
type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t
let rec map_lazy f (Cons (h, t)) = Cons(f h, lazy(map_lazy f (Lazy.force t)))

let rec palindrom = Cons((100, 100), lazy (map_lazy (fun (x, y) -> if y == 999 then (x+1, 100) else (x, y+1)) palindrom))

let rec find_answer_lazy (Cons((x, y), t)) = if x == 999 then 0 else (if is_palindrom (string_of_int (x * y)) then max (x * y) (find_answer_lazy (Lazy.force t)) else (find_answer_lazy (Lazy.force t)))

let _ = print_endline (string_of_int (find_answer_lazy palindrom))