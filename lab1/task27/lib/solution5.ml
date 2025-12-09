open Common

(* with sequence*)
type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t
let rec map f (Cons(h, t)) = Cons(f h, lazy(map f (Lazy.force t)))
let rec sort_through = Cons((-1000, -1000), 
  lazy(map (fun (x, y) -> if y == 1000 then (x + 1, -1000) else (x, y + 1)) sort_through))

let rec find_answer_with_sequence (Cons((a, b), t)) = 
  if a == 1001 then (0, 0) else max ((Common.sort_through_n 0 a b), a * b) (find_answer_with_sequence (Lazy.force t))

let (first_s, second_s) = find_answer_with_sequence sort_through
let _ = print_endline (string_of_int (first_s))
let _ = print_endline(string_of_int (second_s))