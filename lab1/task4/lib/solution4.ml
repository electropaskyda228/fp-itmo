open Common

(* with circles *)
let find_answer_with_circles =
  let acc = ref 0 in
  for a = 100 to 999 do 
    for b = 100 to 999 do 
      if Common.is_palindrome (a * b) then acc := Common.max !acc (a * b)
    done
  done;
  !acc
let _ = print_endline (string_of_int find_answer_with_circles)