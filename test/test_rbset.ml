open Alcotest
open Rbset

let test_empty () =
  check bool "empty set does not contain 1" false (Rbset.member 1 Rbset.empty);
  check bool "empty set does not contain 0" false (Rbset.member 0 Rbset.empty)

let test_insert () =
  let tree = Rbset.insert 1 Rbset.empty in
  check bool "set contains 1 after insertion" true (Rbset.member 1 tree);
  check bool "set does not contain 2" false (Rbset.member 2 tree)

let test_insert_multiple () =
  let tree = Rbset.insert 3 (Rbset.insert 1 (Rbset.insert 2 Rbset.empty)) in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set contains 2" true (Rbset.member 2 tree);
  check bool "set contains 3" true (Rbset.member 3 tree);
  check bool "set does not contain 4" false (Rbset.member 4 tree)

let test_to_list () =
  let tree = Rbset.insert 3 (Rbset.insert 1 (Rbset.insert 2 Rbset.empty)) in
  check (list int) "to_list returns sorted elements" [ 1; 2; 3 ] (Rbset.to_list tree)

let test_insert_duplicates () =
  let tree = Rbset.insert 1 (Rbset.insert 1 Rbset.empty) in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check (list int) "to_list returns unique elements" [ 1 ] (Rbset.to_list tree)

let test_insert_large_numbers () =
  let tree = Rbset.insert 1000000 (Rbset.insert (-1000000) Rbset.empty) in
  check bool "set contains 1000000" true (Rbset.member 1000000 tree);
  check bool "set contains -1000000" true (Rbset.member (-1000000) tree);
  check bool "set does not contain 0" false (Rbset.member 0 tree)

let test_insert_order () =
  let tree = Rbset.insert 2 (Rbset.insert 1 (Rbset.insert 3 Rbset.empty)) in
  check (list int) "to_list returns sorted elements" [ 1; 2; 3 ] (Rbset.to_list tree);
  let tree = Rbset.insert 3 (Rbset.insert 2 (Rbset.insert 1 Rbset.empty)) in
  check (list int) "to_list returns sorted elements" [ 1; 2; 3 ] (Rbset.to_list tree)

let test_insert_negative_numbers () =
  let tree = Rbset.insert (-1) (Rbset.insert (-2) (Rbset.insert (-3) Rbset.empty)) in
  check bool "set contains -1" true (Rbset.member (-1) tree);
  check bool "set contains -2" true (Rbset.member (-2) tree);
  check bool "set contains -3" true (Rbset.member (-3) tree);
  check bool "set does not contain 0" false (Rbset.member 0 tree)

let test_insert_mixed_numbers () =
  let tree = Rbset.insert 0 (Rbset.insert (-1) (Rbset.insert 1 Rbset.empty)) in
  check bool "set contains -1" true (Rbset.member (-1) tree);
  check bool "set contains 0" true (Rbset.member 0 tree);
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set does not contain 2" false (Rbset.member 2 tree)

let () =
  let open Alcotest in
  run "Rbset Tests"
    [
      ("test_empty", [ test_case "test_empty" `Quick test_empty ]);
      ("test_insert", [ test_case "test_insert" `Quick test_insert ]);
      ("test_insert_multiple", [ test_case "test_insert_multiple" `Quick test_insert_multiple ]);
      ("test_to_list", [ test_case "test_to_list" `Quick test_to_list ]);
      ("test_insert_duplicates", [ test_case "test_insert_duplicates" `Quick test_insert_duplicates ]);
      ("test_insert_large_numbers", [ test_case "test_insert_large_numbers" `Quick test_insert_large_numbers ]);
      ("test_insert_order", [ test_case "test_insert_order" `Quick test_insert_order ]);
      ("test_insert_negative_numbers", [ test_case "test_insert_negative_numbers" `Quick test_insert_negative_numbers ]);
      ("test_insert_mixed_numbers", [ test_case "test_insert_mixed_numbers" `Quick test_insert_mixed_numbers ]);
    ]
