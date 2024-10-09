let () =
  let open Alcotest in
  run "Rbset Tests"
    [
      ("test_empty", [ test_case "test_empty" `Quick Test_rbset.test_empty ]);
      ("test_insert", [ test_case "test_insert" `Quick Test_rbset.test_insert ]);
      ("test_insert_multiple", [ test_case "test_insert_multiple" `Quick Test_rbset.test_insert_multiple ]);
      ("test_to_list", [ test_case "test_to_list" `Quick Test_rbset.test_to_list ]);
      ("test_insert_duplicates", [ test_case "test_insert_duplicates" `Quick Test_rbset.test_insert_duplicates ]);
      ("test_insert_large_numbers", [ test_case "test_insert_large_numbers" `Quick Test_rbset.test_insert_large_numbers ]);
      ("test_insert_order", [ test_case "test_insert_order" `Quick Test_rbset.test_insert_order ]);
      ("test_insert_negative_numbers", [ test_case "test_insert_negative_numbers" `Quick Test_rbset.test_insert_negative_numbers ]);
      ("test_insert_mixed_numbers", [ test_case "test_insert_mixed_numbers" `Quick Test_rbset.test_insert_mixed_numbers ]);
    ]
    