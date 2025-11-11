open Alcotest

let test_recursion () =
  check int "" 906609 (Mylib.Solution1.f1 999)

let test_tail () =
  check int "" 906609 (Mylib.Solution2.f1_tr 999 0)

let test_modular () =
  check int "" 906609 (Mylib.Solution3.result)

let test_circles () =
  check int "" 906609 (Mylib.Solution4.find_answer_with_circles)

let test_sequence () =
  check int "" 906609 (Mylib.Solution5.find_answer_lazy Mylib.Solution5.palindrom)

let tests = [
  "recursion", `Quick, test_recursion;
  "tail", `Quick, test_tail;
  "modular", `Quick, test_modular;
  "circles", `Quick, test_circles;
  "sequence", `Quick, test_sequence;
]

let () = 
  run "Test for task4" [
    "Just tests", tests;
  ]  