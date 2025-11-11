open Alcotest

let recursion () =
  let result = Mylib.Solution1.find_answer_with_recursion 1000 1000 in
  check int "" 70 (fst result);
  check int "" (-59231) (snd result)

let tail () =
  let result = Mylib.Solution2.find_answer_with_tail_recursion 1000 1000 in
  check int "" 70 (fst result);
  check int "" (-59231) (snd result)

let modular () =
  let result = Mylib.Solution3.result in
  check int "" 70 (fst result);
  check int "" (-59231) (snd result)

let circles () =
  check int "" 70 Mylib.Solution4.find_answer_with_circles

let sequence () =
  let result = Mylib.Solution5.find_answer_with_sequence Mylib.Solution5.sort_through in
  check int "" 70 (fst result);
  check int "" (-59231) (snd result)

let tests = [
  "recursion", `Quick, recursion;
  "tail", `Quick, tail;
  "modular", `Quick, modular;
  "circles", `Quick, circles;
  "sequence", `Quick, sequence;
]

let () = 
  run "Test for task" [
    "Just tests", tests;
  ] 