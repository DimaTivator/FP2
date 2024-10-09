open OUnit2
open Rbset

let test_empty _ =
  assert_equal false (Rbset.member 1 Rbset.empty);
  assert_equal false (Rbset.member 0 Rbset.empty)

let test_insert _ =
  let tree = Rbset.insert 1 Rbset.empty in
  assert_equal true (Rbset.member 1 tree);
  assert_equal false (Rbset.member 2 tree)

let test_insert_multiple _ =
  let tree = Rbset.insert 3 (Rbset.insert 1 (Rbset.insert 2 Rbset.empty)) in
  assert_equal true (Rbset.member 1 tree);
  assert_equal true (Rbset.member 2 tree);
  assert_equal true (Rbset.member 3 tree);
  assert_equal false (Rbset.member 4 tree)

let test_to_list _ =
  let tree = Rbset.insert 3 (Rbset.insert 1 (Rbset.insert 2 Rbset.empty)) in
  assert_equal [ 1; 2; 3 ] (Rbset.to_list tree)

let test_insert_duplicates _ =
  let tree = Rbset.insert 1 (Rbset.insert 1 Rbset.empty) in
  assert_equal true (Rbset.member 1 tree);
  assert_equal [ 1 ] (Rbset.to_list tree)

let test_insert_large_numbers _ =
  let tree = Rbset.insert 1000000 (Rbset.insert (-1000000) Rbset.empty) in
  assert_equal true (Rbset.member 1000000 tree);
  assert_equal true (Rbset.member (-1000000) tree);
  assert_equal false (Rbset.member 0 tree)

let test_insert_order _ =
  let tree = Rbset.insert 2 (Rbset.insert 1 (Rbset.insert 3 Rbset.empty)) in
  assert_equal [ 1; 2; 3 ] (Rbset.to_list tree);
  let tree = Rbset.insert 3 (Rbset.insert 2 (Rbset.insert 1 Rbset.empty)) in
  assert_equal [ 1; 2; 3 ] (Rbset.to_list tree)

let suite =
  "Rbset Tests"
  >::: [
         "test_empty" >:: test_empty;
         "test_insert" >:: test_insert;
         "test_insert_multiple" >:: test_insert_multiple;
         "test_to_list" >:: test_to_list;
         "test_insert_duplicates" >:: test_insert_duplicates;
         "test_insert_large_numbers" >:: test_insert_large_numbers;
         "test_insert_order" >:: test_insert_order;
       ]
