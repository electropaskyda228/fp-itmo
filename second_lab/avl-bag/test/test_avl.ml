open Alcotest

module AM = Avl_bag.AvlMultiset.AvlMultiset

let make_test_tree = 
  AM.empty |>
  AM.insert 9 |>
  AM.insert 8 |> AM.insert 8 |> AM.insert 8 |>
  AM.insert 7 |> AM.insert 7 |>
  AM.insert 6 |>
  AM.insert 5 |>
  AM.insert 4 |> AM.insert 4 |>
  AM.insert 3 |>
  AM.insert 2 |> AM.insert 2 |> AM.insert 2 |> AM.insert 2 |>
  AM.insert 1

let avl_arb_simple =
  QCheck.(Gen.(list small_int |> map (fun lst ->
    List.fold_left (fun t x -> AM.insert x t) AM.empty lst))
    |> make)

let test_empty () =
  let t = AM.empty in
  Alcotest.(check int) "empty total_size" 0 (AM.total_size t)

let test_total_size () =
  let tree = make_test_tree in
  Alcotest.(check int) "total_size method" 16 (AM.total_size tree)

let test_unique_size () =
  let tree = make_test_tree in
  Alcotest.(check int) "unique_size method" 9 (AM.unique_size tree)

let test_contains () =
  let tree = make_test_tree in
  Alcotest.(check bool) "contains method true" true (AM.contains 9 tree);
  Alcotest.(check bool) "contains method false" false (AM.contains 10 tree)

let test_delete () =
  let tree = make_test_tree in
  Alcotest.(check bool) "delete complete" false (AM.contains 1 (AM.delete 1 tree));
  Alcotest.(check int) "delete uncomplete" 3 (AM.count 2 (AM.delete 2 tree));
  Alcotest.(check bool) "delete_ghost" false (AM.contains 10 (AM.delete 10 tree))

let test_map () =
  let tree = make_test_tree in
  let double x = 2 * x in
  let changed_tree = (AM.map double tree) in
  Alcotest.(check bool) "map method top" true (AM.contains 18 changed_tree);
  Alcotest.(check bool) "map method bottom" true (AM.contains 2 changed_tree);
  Alcotest.(check bool) "map method check count" true ((AM.count 14 changed_tree) == (AM.count 7 tree))

let test_filter () =
  let tree = make_test_tree in
  let even x = x mod 2 == 0 in
  let changed_tree = AM.filter even tree in
  Alcotest.(check int) "filter method" 4 (AM.unique_size changed_tree)

let test_fold_left () =
  let tree = make_test_tree in
  let sum = AM.fold_left (fun acc v cnt -> acc + (v * cnt)) 0 tree in
  Alcotest.(check int) "fold_left method" 78 sum

let test_fold_right () =
  let tree = make_test_tree in
  let result2 = AM.fold_right (fun v cnt acc -> (v * cnt) - acc) tree 0 in
  Alcotest.(check int) "fold_right method" (-14) result2

let test_equal () =
  let tree1 = make_test_tree in
  let tree2 = make_test_tree in
  Alcotest.(check bool) "equal" true (tree1 = tree2)

let test_monoid_union () =
  let tree = make_test_tree in
  let tree1 = AM.union AM.empty tree in
  let tree2 = AM.union tree AM.empty in
  let df_tree = AM.insert 10 tree in
  let tree3 = AM.union df_tree tree in
  let tree4 = AM.union tree df_tree in
  Alcotest.(check bool) "ordinary" true ((tree1 = tree2) && (tree1 = tree));
  Alcotest.(check bool) "assoc" true (tree3 = tree4)

let test_commute () =
  let a = make_test_tree in
  let b = AM.empty |> AM.insert 1 in
  let c = AM.empty |> AM.insert 2 in
  let left_assoc = AM.union (AM.union a b) c in
  let right_assoc = AM.union a (AM.union b c) in
  Alcotest.(check bool) "associativity of union" true (left_assoc = right_assoc)

(* Property-based *)
let prop_union_assoc =
  QCheck.Test.make ~count:1000 ~name:"union associative"
    QCheck.(triple avl_arb_simple avl_arb_simple avl_arb_simple)
    (fun (a, b, c) ->
      AM.equal (AM.union (AM.union a b) c) 
               (AM.union a (AM.union b c)))

let prop_intersection_min =
  QCheck.Test.make ~count:1000 ~name:"intersection takes min counts"
    (QCheck.pair avl_arb_simple avl_arb_simple)
    (fun (a, b) ->
      let inter_ab = AM.intersection a b in
      let all_values = 
        AM.fold_left (fun acc v cnt -> (v, cnt) :: acc) [] a @
        AM.fold_left (fun acc v cnt -> (v, cnt) :: acc) [] b in
      let values = List.fold_left (fun acc (v, _) -> if List.mem v acc then acc else v :: acc) [] all_values in
      List.for_all (fun x ->
        let count_a = AM.count x a in
        let count_b = AM.count x b in
        let count_inter = AM.count x inter_ab in
        count_inter = min count_a count_b
      ) values)

let prop_difference_sub =
  QCheck.Test.make ~count:1000 ~name:"difference subtracts counts"
    (QCheck.pair avl_arb_simple avl_arb_simple)
    (fun (a, b) ->
      let diff_ab = AM.difference a b in
      let all_values = 
        AM.fold_left (fun acc v cnt -> (v, cnt) :: acc) [] a @
        AM.fold_left (fun acc v cnt -> (v, cnt) :: acc) [] b in
      let values = List.fold_left (fun acc (v, _) -> if List.mem v acc then acc else v :: acc) [] all_values in
      List.for_all (fun x ->
        let count_a = AM.count x a in
        let count_b = AM.count x b in
        let count_diff = AM.count x diff_ab in
        count_diff = max 0 (count_a - count_b)
      ) values)

let prop_insert_count =
  QCheck.Test.make ~count:1000 ~name:"insert increases count by 1"
    (QCheck.pair QCheck.small_int avl_arb_simple)
    (fun (x, t) -> 
      let new_t = AM.insert x t in
      AM.count x new_t = AM.count x t + 1)

let prop_delete_decreases =
  QCheck.Test.make ~count:1000 ~name:"delete decreases count by 1 or removes"
    (QCheck.pair QCheck.small_int avl_arb_simple)
    (fun (x, t) -> 
      let new_t = AM.delete x t in
      let old_count = AM.count x t in
      old_count = 0 || AM.count x new_t = old_count - 1)

let () =
  Alcotest.run "AVL Tests" [
    ("Basic", [
      test_case "empty" `Quick test_empty;
      test_case "total_size" `Quick test_total_size;
      test_case "unique_size" `Quick test_unique_size;
      test_case "contains" `Quick test_contains;
      test_case "delete" `Quick test_delete;
      test_case "map" `Quick test_map;
      test_case "filter" `Quick test_filter;
      test_case "fold_left" `Quick test_fold_left;
      test_case "fold_right" `Quick test_fold_right;
      test_case "equal" `Quick test_equal;
      test_case "monoid_union" `Quick test_monoid_union;
      test_case "commute" `Quick test_commute;
    ]);
    ("Property_based", [
      QCheck_alcotest.to_alcotest prop_union_assoc;
      QCheck_alcotest.to_alcotest prop_intersection_min;
      QCheck_alcotest.to_alcotest prop_difference_sub;
      QCheck_alcotest.to_alcotest prop_insert_count;
      QCheck_alcotest.to_alcotest prop_delete_decreases;
    ])
  ]