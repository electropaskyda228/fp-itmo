open Alcotest

let test_recursion () =
  check int "" 906609 (Mylib.Solution1.f1 999 0)

let test_tail () =
  check int "" 906609 (Mylib.Solution2.f1_tr 999 999 0)

let test_map_generate () =
  check int "" 906609 (Mylib.Solution3.result)

let test_circles () =
  check int "" 906609 (Mylib.Solution4.find_answer_with_circles)

let test_sequence () =
  check int "" 906609 (Mylib.Solution5.find_answer_lazy Mylib.Solution5.palindrom)

let test_modular () =
  check int "" 906609 (Mylib.Solution6.PalindromeSolver.solve 3 3)

let tests = [
  "recursion", `Quick, test_recursion;
  "tail", `Quick, test_tail;
  "map generate", `Quick, test_map_generate;
  "circles", `Quick, test_circles;
  "sequence", `Quick, test_sequence;
  "modular", `Quick, test_modular;
]

let () = 
  run "Test for task4" [
    "Just tests", tests;
  ]  
