open Common

(* with circles *)
let find_answer_with_circles =
  let r = ref (0, 0) in
  for a = -1000 to 1000 do
    for b = -1000 to 1000 do
      let n = ref 0 in
      while Common.is_prime (Common.f !n a b) do
        n := !n + 1 
      done;
      if fst !r < !n then r := (!n - 1, a * b)
    done
  done;
  fst !r
let _ = print_endline (string_of_int (find_answer_with_circles))