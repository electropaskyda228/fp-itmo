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

let test_empty () =
  let t = AM.empty in
  check int "empty total_size" 0 (AM.total_size t)

let test_total_size () =
  let tree = make_test_tree in
  check int "total_size method" 16 (AM.total_size tree)

let test_unique_size () =
  let tree = make_test_tree in
  check int "unique_size method" 9 (AM.unique_size tree)

let test_contains () =
  let tree = make_test_tree in
  check bool "contains method true" true (AM.contains 9 tree);
  check bool "contains method false" false (AM.contains 10 tree)

let test_delete () =
  let tree = make_test_tree in
  check bool "delete complete" false (AM.contains 1 (AM.delete 1 tree));
  check int "delete uncomplete" 3 (AM.count 2 (AM.delete 2 tree));
  check bool "delete_ghost" false (AM.contains 10 (AM.delete 10 tree))

let test_map () =
  let tree = make_test_tree in
  let double x = 2 * x in
  let changed_tree = (AM.map double tree) in
  check bool "map method top" true (AM.contains 18 changed_tree);
  check bool "map method bottom" true (AM.contains 2 changed_tree);
  check bool "map method check count" true ((AM.count 14 changed_tree) == (AM.count 7 tree))

let test_filter () =
  let tree = make_test_tree in
  let even x = x mod 2 == 0 in
  let changed_tree = AM.filter even tree in
  check int "filter method" 4 (AM.unique_size changed_tree)

let test_fold_left () =
  let tree = make_test_tree in
  let sum = AM.fold_left (fun acc v cnt -> acc + (v * cnt)) 0 tree in
  check int "fold_left method" 78 sum

let test_fold_right () =
  let tree = make_test_tree in
  let result2 = AM.fold_right (fun v cnt acc -> (v * cnt) - acc) tree 0 in
  check int "fold_right method" (-14) result2

let test_equal () =
  let tree1 = make_test_tree in
  let tree2 = make_test_tree in
  check bool "equal" true (tree1 = tree2)

let test_monoid_union () =
  let tree = make_test_tree in
  let tree1 = AM.union AM.empty tree in
  let tree2 = AM.union tree AM.empty in
  let df_tree = AM.insert 10 tree in
  let tree3 = AM.union df_tree tree in
  let tree4 = AM.union tree df_tree in
  check bool "ordinary" true ((tree1 = tree2) && (tree1 = tree));
  check bool "assoc" true (tree3 = tree4)

let test_commute () =
  let a = make_test_tree in
  let b = AM.empty |> AM.insert 1 in
  let c = AM.empty |> AM.insert 2 in
  let left_assoc = AM.union (AM.union a b) c in
  let right_assoc = AM.union a (AM.union b c) in
  check bool "associativity of union" true (left_assoc = right_assoc)

let test_de_morgan () =
  let tree = make_test_tree in
  let tree1 = AM.empty |> AM.insert 1 |> AM.insert 2 |> AM.insert 2 |> AM.insert 2 |> AM.insert 2 in
  let tree2 = AM.empty |> AM.insert 3 |> AM.insert 4 |> AM.insert 4 in
  let demorgan_left = AM.difference tree (AM.union tree1 tree2) in
  let demorgan_right = AM.intersection (AM.difference tree tree1) (AM.difference tree tree2) in
  check bool "De Morgan's law" true (demorgan_left = demorgan_right)


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
      test_case "equal" `Quick test_equal
    ]);
    ("Property_based", [
      test_case "monoid_union" `Quick test_monoid_union;
      test_case "commute" `Quick test_commute;
      test_case "de_morgan" `Quick test_de_morgan
    ])
  ]
