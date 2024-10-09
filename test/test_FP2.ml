open OUnit2

let () =
  let suite = "MainTestSuite" >::: [ Test_rbset.suite ] in
  run_test_tt_main suite
