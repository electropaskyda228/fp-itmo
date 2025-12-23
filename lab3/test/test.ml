open Mylib

module Interpolation = Mylib.Interpolation

let test_points = [
  Common.Pair(1.0, 1.0);
  Common.Pair(2.0, 2.0);
  Common.Pair(3.0, 3.0);
  Common.Pair(4.0, 4.0)
]

let test_points_nonlinear = [
  Common.Pair(0.0, 0.0);
  Common.Pair(1.0, 1.0);
  Common.Pair(2.0, 4.0);
  Common.Pair(3.0, 9.0)
]

let float_pair_equal (Common.Pair(x1, y1)) (Common.Pair(x2, y2)) =
  abs_float (x1 -. x2) < 0.001 && abs_float (y1 -. y2) < 0.001

let test_linear_interpolate_basic () =
  let delta = 0.5 in
  let result = Interpolation.linear_interpolate test_points delta in
  
  let expected = [
    Common.Pair(1.0, 1.0);
    Common.Pair(1.5, 1.5);
    Common.Pair(2.0, 2.0);
    Common.Pair(2.5, 2.5);
    Common.Pair(3.0, 3.0);
    Common.Pair(3.5, 3.5);
    Common.Pair(4.0, 4.0)
  ] in
  
  Alcotest.(check (list (testable Common.pp_pair float_pair_equal)))
    "linear interpolate basic" expected result

let test_linear_interpolate_single_point () =
  let delta = 0.5 in
  let single_point = [Common.Pair(1.0, 1.0)] in
  let result = Interpolation.linear_interpolate single_point delta in
  
  Alcotest.(check (list (testable Common.pp_pair float_pair_equal)))
    "linear interpolate single point" [] result

let test_linear_interpolate_empty () =
  let delta = 0.5 in
  let result = Interpolation.linear_interpolate [] delta in
  
  Alcotest.(check (list (testable Common.pp_pair float_pair_equal)))
    "linear interpolate empty" [] result

let test_linear_interpolate_reverse_order () =
  let delta = 0.5 in
  let reversed_points = List.rev test_points in
  let result = Interpolation.linear_interpolate reversed_points delta in
  
  let expected = [
    Common.Pair(1.0, 1.0);
    Common.Pair(1.5, 1.5);
    Common.Pair(2.0, 2.0);
    Common.Pair(2.5, 2.5);
    Common.Pair(3.0, 3.0);
    Common.Pair(3.5, 3.5);
    Common.Pair(4.0, 4.0)
  ] in
  
  Alcotest.(check (list (testable Common.pp_pair float_pair_equal)))
    "linear interpolate reverse order" expected result

let test_linear_interpolate_nonlinear () =
  let delta = 0.5 in
  let result = Interpolation.linear_interpolate test_points_nonlinear delta in
  
  let expected = [
    Common.Pair(0.0, 0.0);
    Common.Pair(0.5, 0.5);
    Common.Pair(1.0, 1.0);
    Common.Pair(1.5, 2.5);
    Common.Pair(2.0, 4.0);
    Common.Pair(2.5, 6.5);
    Common.Pair(3.0, 9.0)
  ] in
  
  Alcotest.(check (list (testable Common.pp_pair float_pair_equal)))
    "linear interpolate nonlinear" expected result

(* Тесты для Ньютона *)
let test_newton_interpolate_basic () =
  let delta = 0.5 in
  let result = Interpolation.newton_interpolate test_points delta in
  
  let expected = [
    Common.Pair(1.0, 1.0);
    Common.Pair(1.5, 1.5);
    Common.Pair(2.0, 2.0);
    Common.Pair(2.5, 2.5);
    Common.Pair(3.0, 3.0);
    Common.Pair(3.5, 3.5);
    Common.Pair(4.0, 4.0)
  ] in
  
  Alcotest.(check (list (testable Common.pp_pair float_pair_equal)))
    "newton interpolate basic" expected result

let test_newton_interpolate_nonlinear () =
  let delta = 1.0 in
  let result = Interpolation.newton_interpolate test_points_nonlinear delta in
  
  let rec check_points = function
    | [] -> true
    | Common.Pair(x, y) :: rest ->
        let expected_y = x *. x in
        abs_float (y -. expected_y) < 0.001 && check_points rest
  in
  
  let all_correct = check_points result in
  Alcotest.(check bool) "newton interpolate nonlinear" true all_correct

let test_newton_interpolate_single_point () =
  let delta = 0.5 in
  let single_point = [Common.Pair(1.0, 1.0)] in
  let result = Interpolation.newton_interpolate single_point delta in
  
  Alcotest.(check (list (testable Common.pp_pair float_pair_equal)))
    "newton interpolate single point" [] result

let test_newton_interpolate_empty () =
  let delta = 0.5 in
  let result = Interpolation.newton_interpolate [] delta in
  
  Alcotest.(check (list (testable Common.pp_pair float_pair_equal)))
    "newton interpolate empty" [] result

let test_newton_interpolate_three_points () =
  let delta = 0.5 in
  let points = [
    Common.Pair(0.0, 0.0);
    Common.Pair(1.0, 1.0);
    Common.Pair(2.0, 4.0)
  ] in
  let result = Interpolation.newton_interpolate points delta in
  
  let expected_x = [0.0; 0.5; 1.0; 1.5; 2.0] in
  let expected_y = [0.0; 0.25; 1.0; 2.25; 4.0] in
  
  let rec check_all xs ys = function
    | [] -> xs = [] && ys = []
    | Common.Pair(x, y) :: rest ->
        match xs, ys with
        | x' :: xs', y' :: ys' ->
            abs_float (x -. x') < 0.001 && 
            abs_float (y -. y') < 0.001 &&
            check_all xs' ys' rest
        | _ -> false
  in
  
  let all_correct = check_all expected_x expected_y result in
  Alcotest.(check bool) "newton interpolate three points" true all_correct

let test_compare_linear_newton () =
  let delta = 0.25 in
  let linear_result = Interpolation.linear_interpolate test_points delta in
  let newton_result = Interpolation.newton_interpolate test_points delta in
  
  let rec compare_lists l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | p1 :: rest1, p2 :: rest2 ->
        float_pair_equal p1 p2 && compare_lists rest1 rest2
    | _ -> false
  in
  
  let equal = compare_lists linear_result newton_result in
  Alcotest.(check bool) "linear and newton equal for linear data" true equal

let test_sort_by_x () =
  let unsorted = [
    Common.Pair(3.0, 3.0);
    Common.Pair(1.0, 1.0);
    Common.Pair(4.0, 4.0);
    Common.Pair(2.0, 2.0)
  ] in
  
  let sorted = Interpolation.sort_by_x unsorted in
  
  let expected = [
    Common.Pair(1.0, 1.0);
    Common.Pair(2.0, 2.0);
    Common.Pair(3.0, 3.0);
    Common.Pair(4.0, 4.0)
  ] in
  
  Alcotest.(check (list (testable Common.pp_pair float_pair_equal)))
    "sort by x" expected sorted

let interpolation_tests = [
  ("linear_basic", `Quick, test_linear_interpolate_basic);
  ("linear_single", `Quick, test_linear_interpolate_single_point);
  ("linear_empty", `Quick, test_linear_interpolate_empty);
  ("linear_reverse", `Quick, test_linear_interpolate_reverse_order);
  ("linear_nonlinear", `Quick, test_linear_interpolate_nonlinear);
  
  ("newton_basic", `Quick, test_newton_interpolate_basic);
  ("newton_nonlinear", `Quick, test_newton_interpolate_nonlinear);
  ("newton_single", `Quick, test_newton_interpolate_single_point);
  ("newton_empty", `Quick, test_newton_interpolate_empty);
  ("newton_three_points", `Quick, test_newton_interpolate_three_points);
  
  ("compare_methods", `Quick, test_compare_linear_newton);
  ("sort_points", `Quick, test_sort_by_x);
]

let () =
  Alcotest.run "Interpolation Tests" [
    ("Interpolation", interpolation_tests)
  ]