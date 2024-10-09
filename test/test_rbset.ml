open Alcotest
open Rbset

let test_empty () =
  check bool "empty set does not contain 1" false (Rbset.member 1 Rbset.empty);
  check bool "empty set does not contain 0" false (Rbset.member 0 Rbset.empty)

let test_insert () =
  let tree = Rbset.empty |> Rbset.insert 1 in
  check bool "set contains 1 after insertion" true (Rbset.member 1 tree);
  check bool "set does not contain 2" false (Rbset.member 2 tree)

let test_insert_multiple () =
  let tree =
    Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3
  in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set contains 2" true (Rbset.member 2 tree);
  check bool "set contains 3" true (Rbset.member 3 tree);
  check bool "set does not contain 4" false (Rbset.member 4 tree)

let test_to_list () =
  let tree =
    Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3
  in
  check (list int) "to_list returns sorted elements" [ 1; 2; 3 ]
    (Rbset.to_list tree)

let test_insert_duplicates () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 1 in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check (list int) "to_list returns unique elements" [ 1 ] (Rbset.to_list tree)

let test_insert_order () =
  let tree =
    Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3
  in
  check (list int) "to_list returns sorted elements" [ 1; 2; 3 ]
    (Rbset.to_list tree);
  let tree =
    Rbset.empty |> Rbset.insert 3 |> Rbset.insert 2 |> Rbset.insert 1
  in
  check (list int) "to_list returns sorted elements" [ 1; 2; 3 ]
    (Rbset.to_list tree)

let test_insert_negative_numbers () =
  let tree =
    Rbset.empty |> Rbset.insert (-1) |> Rbset.insert (-2) |> Rbset.insert (-3)
  in
  check bool "set contains -1" true (Rbset.member (-1) tree);
  check bool "set contains -2" true (Rbset.member (-2) tree);
  check bool "set contains -3" true (Rbset.member (-3) tree);
  check bool "set does not contain 0" false (Rbset.member 0 tree)

let test_insert_mixed_numbers () =
  let tree =
    Rbset.empty |> Rbset.insert (-1) |> Rbset.insert 0 |> Rbset.insert 1
  in
  check bool "set contains -1" true (Rbset.member (-1) tree);
  check bool "set contains 0" true (Rbset.member 0 tree);
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set does not contain 2" false (Rbset.member 2 tree)

let test_insert_and_check_order () =
  let tree =
    Rbset.empty |> Rbset.insert 10 |> Rbset.insert 20 |> Rbset.insert 30
    |> Rbset.insert 15 |> Rbset.insert 25
  in
  check bool "set contains 10" true (Rbset.member 10 tree);
  check bool "set contains 15" true (Rbset.member 15 tree);
  check bool "set contains 20" true (Rbset.member 20 tree);
  check bool "set contains 25" true (Rbset.member 25 tree);
  check bool "set contains 30" true (Rbset.member 30 tree);
  check bool "set does not contain -30" false (Rbset.member (-30) tree);
  check (list int) "to_list returns sorted elements" [ 10; 15; 20; 25; 30 ]
    (Rbset.to_list tree)

let test_remove () =
  let tree =
    Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3
    |> Rbset.remove 2
  in
  check bool "set does not contain 2 after removal" false (Rbset.member 2 tree);
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set contains 3" true (Rbset.member 3 tree);
  check (list int) "to_list returns sorted elements" [ 1; 3 ]
    (Rbset.to_list tree)

let test_remove_nonexistent () =
  let tree =
    Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.remove 3
  in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set contains 2" true (Rbset.member 2 tree);
  check bool "set does not contain 3" false (Rbset.member 3 tree);
  check (list int) "to_list returns sorted elements" [ 1; 2 ]
    (Rbset.to_list tree)

let test_insert_and_remove () =
  let tree =
    Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3
    |> Rbset.remove 2 |> Rbset.insert 4
  in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set does not contain 2" false (Rbset.member 2 tree);
  check bool "set contains 3" true (Rbset.member 3 tree);
  check bool "set contains 4" true (Rbset.member 4 tree);
  check (list int) "to_list returns sorted elements" [ 1; 3; 4 ]
    (Rbset.to_list tree)

let test_remove_all () =
  let tree =
    Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3
    |> Rbset.remove 1 |> Rbset.remove 2 |> Rbset.remove 3
  in
  check bool "set does not contain 1" false (Rbset.member 1 tree);
  check bool "set does not contain 2" false (Rbset.member 2 tree);
  check bool "set does not contain 3" false (Rbset.member 3 tree);
  check (list int) "to_list returns empty list" [] (Rbset.to_list tree)

let run_tests =
  let open Alcotest in
  run "Rbset Tests"
    [
      ("test_empty", [ test_case "test_empty" `Quick test_empty ]);
      ("test_insert", [ test_case "test_insert" `Quick test_insert ]);
      ( "test_insert_multiple",
        [ test_case "test_insert_multiple" `Quick test_insert_multiple ] );
      ("test_to_list", [ test_case "test_to_list" `Quick test_to_list ]);
      ( "test_insert_duplicates",
        [ test_case "test_insert_duplicates" `Quick test_insert_duplicates ] );
      ( "test_insert_order",
        [ test_case "test_insert_order" `Quick test_insert_order ] );
      ( "test_insert_negative_numbers",
        [
          test_case "test_insert_negative_numbers" `Quick
            test_insert_negative_numbers;
        ] );
      ( "test_insert_mixed_numbers",
        [
          test_case "test_insert_mixed_numbers" `Quick test_insert_mixed_numbers;
        ] );
      ( "test_insert_and_remove",
        [ test_case "test_insert_and_remove" `Quick test_insert_and_remove ] );
      ( "test_insert_and_check_order",
        [
          test_case "test_insert_and_check_order" `Quick
            test_insert_and_check_order;
        ] );
      ("test_remove", [ test_case "test_remove" `Quick test_remove ]);
      ( "test_remove_nonexistent",
        [ test_case "test_remove_nonexistent" `Quick test_remove_nonexistent ]
      );
      ("test_remove_all", [ test_case "test_remove_all" `Quick test_remove_all ]);
    ]
