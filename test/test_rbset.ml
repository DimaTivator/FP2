open Alcotest
open Rbset

let test_empty () =
  check bool "empty set does not contain 1" false (Rbset.member 1 Rbset.empty);
  check bool "empty set does not contain 0" false (Rbset.member 0 Rbset.empty)
;;

let test_insert () =
  let tree = Rbset.empty |> Rbset.insert 1 in
  check bool "set contains 1 after insertion" true (Rbset.member 1 tree);
  check bool "set does not contain 2" false (Rbset.member 2 tree)
;;

let test_insert_multiple () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3 in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set contains 2" true (Rbset.member 2 tree);
  check bool "set contains 3" true (Rbset.member 3 tree);
  check bool "set does not contain 4" false (Rbset.member 4 tree)
;;

let test_to_list () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3 in
  check (list int) "to_list returns sorted elements" [ 1; 2; 3 ] (Rbset.to_list tree)
;;

let test_insert_duplicates () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 1 in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check (list int) "to_list returns unique elements" [ 1 ] (Rbset.to_list tree)
;;

let test_insert_order () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3 in
  check (list int) "to_list returns sorted elements" [ 1; 2; 3 ] (Rbset.to_list tree);
  let tree = Rbset.empty |> Rbset.insert 3 |> Rbset.insert 2 |> Rbset.insert 1 in
  check (list int) "to_list returns sorted elements" [ 1; 2; 3 ] (Rbset.to_list tree)
;;

let test_insert_and_check_order () =
  let tree =
    Rbset.empty
    |> Rbset.insert 10
    |> Rbset.insert 20
    |> Rbset.insert 30
    |> Rbset.insert 15
    |> Rbset.insert 25
  in
  check bool "set contains 10" true (Rbset.member 10 tree);
  check bool "set contains 15" true (Rbset.member 15 tree);
  check bool "set contains 20" true (Rbset.member 20 tree);
  check bool "set contains 25" true (Rbset.member 25 tree);
  check bool "set contains 30" true (Rbset.member 30 tree);
  check bool "set does not contain -30" false (Rbset.member (-30) tree);
  check
    (list int)
    "to_list returns sorted elements"
    [ 10; 15; 20; 25; 30 ]
    (Rbset.to_list tree)
;;

let test_remove () =
  let tree =
    Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3 |> Rbset.remove 2
  in
  check bool "set does not contain 2 after removal" false (Rbset.member 2 tree);
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set contains 3" true (Rbset.member 3 tree);
  check (list int) "to_list returns sorted elements" [ 1; 3 ] (Rbset.to_list tree)
;;

let test_remove_nonexistent () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.remove 3 in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set contains 2" true (Rbset.member 2 tree);
  check bool "set does not contain 3" false (Rbset.member 3 tree);
  check (list int) "to_list returns sorted elements" [ 1; 2 ] (Rbset.to_list tree)
;;

let test_insert_and_remove () =
  let tree =
    Rbset.empty
    |> Rbset.insert 1
    |> Rbset.insert 2
    |> Rbset.insert 3
    |> Rbset.remove 2
    |> Rbset.insert 4
  in
  check bool "set contains 1" true (Rbset.member 1 tree);
  check bool "set does not contain 2" false (Rbset.member 2 tree);
  check bool "set contains 3" true (Rbset.member 3 tree);
  check bool "set contains 4" true (Rbset.member 4 tree);
  check (list int) "to_list returns sorted elements" [ 1; 3; 4 ] (Rbset.to_list tree)
;;

let test_remove_all () =
  let tree =
    Rbset.empty
    |> Rbset.insert 1
    |> Rbset.insert 2
    |> Rbset.insert 3
    |> Rbset.remove 1
    |> Rbset.remove 2
    |> Rbset.remove 3
  in
  check bool "set does not contain 1" false (Rbset.member 1 tree);
  check bool "set does not contain 2" false (Rbset.member 2 tree);
  check bool "set does not contain 3" false (Rbset.member 3 tree);
  check (list int) "to_list returns empty list" [] (Rbset.to_list tree)
;;

let test_fold () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3 in
  let sum = Rbset.fold ( + ) 0 tree in
  check int "fold sums the elements" 6 sum;
  let product = Rbset.fold ( * ) 1 tree in
  check int "fold multiplies the elements" 6 product
;;

let test_map () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3 in
  let mapped_tree = Rbset.map (fun x -> x * 2) tree in
  check (list int) "map doubles the elements" [ 2; 4; 6 ] (Rbset.to_list mapped_tree);
  let mapped_tree = Rbset.map (fun x -> x + 1) tree in
  check (list int) "map increments the elements" [ 2; 3; 4 ] (Rbset.to_list mapped_tree)
;;

let test_filter () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 |> Rbset.insert 3 in
  let filtered_tree = Rbset.filter (fun x -> x mod 2 = 0) tree in
  check (list int) "filter keeps even elements" [ 2 ] (Rbset.to_list filtered_tree);
  let filtered_tree = Rbset.filter (fun x -> x > 1) tree in
  check
    (list int)
    "filter keeps elements greater than 1"
    [ 2; 3 ]
    (Rbset.to_list filtered_tree)
;;

(* Property-based tests using QCheck *)
let arb_tree =
  let open QCheck in
  let rec gen_tree n =
    match n with
    | 0 -> Gen.return Rbset.empty
    | _ ->
      Gen.frequency
        [ 1, Gen.return Rbset.empty
        ; 3, Gen.map2 (fun x t -> Rbset.insert x t) Gen.int (gen_tree (n - 1))
        ]
  in
  make ~print:(fun tree -> Print.(list int) (Rbset.to_list tree)) (gen_tree 10)
;;

let test_insert_property =
  QCheck.Test.make ~name:"inserted element is a member" arb_tree (fun tree ->
    let x = QCheck.Gen.generate1 QCheck.Gen.int in
    Rbset.member x (Rbset.insert x tree))
;;

let test_remove_property =
  QCheck.Test.make ~name:"removed element is not a member" arb_tree (fun tree ->
    let x = QCheck.Gen.generate1 QCheck.Gen.int in
    not (Rbset.member x (Rbset.remove x tree)))
;;

let test_union_property =
  QCheck.Test.make
    ~name:"union contains all elements"
    QCheck.(pair arb_tree arb_tree)
    (fun (t1, t2) ->
      let union_tree = Rbset.union t1 t2 in
      List.for_all (fun x -> Rbset.member x union_tree) (Rbset.to_list t1)
      && List.for_all (fun x -> Rbset.member x union_tree) (Rbset.to_list t2))
;;

module IntSet = Set.Make (Int)

(* Property-based tests comparing with built-in Set *)

let arb_operations =
  let open QCheck in
  let gen_op =
    Gen.oneof
      [ Gen.map (fun x -> `Insert x) Gen.int
      ; Gen.map (fun x -> `Remove x) Gen.int
      ; Gen.return `To_list
      ]
  in
  let rec gen_ops n =
    match n with
    | 0 -> Gen.return []
    | _ -> Gen.map2 (fun op ops -> op :: ops) gen_op (gen_ops (n - 1))
  in
  make
    ~print:(fun ops ->
      Print.(list string)
        (List.map
           (function
             | `Insert x -> "Insert " ^ string_of_int x
             | `Remove x -> "Remove " ^ string_of_int x
             | `To_list -> "To_list")
           ops))
    (gen_ops 100)
;;

let apply_operations ops =
  let rec aux ops rbset intset =
    match ops with
    | [] -> rbset, intset
    | `Insert x :: rest -> aux rest (Rbset.insert x rbset) (IntSet.add x intset)
    | `Remove x :: rest -> aux rest (Rbset.remove x rbset) (IntSet.remove x intset)
    | `To_list :: rest -> aux rest rbset intset
  in
  aux ops Rbset.empty IntSet.empty
;;

let test_operations_property =
  QCheck.Test.make
    ~name:"operations produce same result as built-in set"
    arb_operations
    (fun ops ->
       let rbset, intset = apply_operations ops in
       Rbset.to_list rbset = IntSet.elements intset)
;;

(* Monoid properties *)
let test_monoid_identity () =
  let tree = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 2 in
  check
    (list int)
    "union with empty is identity"
    (Rbset.to_list tree)
    (Rbset.to_list (Rbset.union tree Rbset.empty));
  check
    (list int)
    "union with empty is identity"
    (Rbset.to_list tree)
    (Rbset.to_list (Rbset.union Rbset.empty tree))
;;

let test_monoid_associativity () =
  let t1 = Rbset.empty |> Rbset.insert 1 |> Rbset.insert 5 |> Rbset.insert 14 in
  let t2 = Rbset.empty |> Rbset.insert 2 |> Rbset.insert (-2) |> Rbset.insert (-5) in
  let t3 = Rbset.empty |> Rbset.insert 3 |> Rbset.insert 5 in
  let union1 = Rbset.union t1 (Rbset.union t2 t3) in
  let union2 = Rbset.union (Rbset.union t1 t2) t3 in
  check (list int) "union is associative" (Rbset.to_list union1) (Rbset.to_list union2)
;;

let is_red_black_tree tree =
  let rec check_black_height node =
    match node with
    | Rbset.Empty -> Some 0
    | Rbset.Node (color, left, _, right) ->
      let left_height = check_black_height left in
      let right_height = check_black_height right in
      (match left_height, right_height with
       | Some lh, Some rh when lh = rh ->
         if color = Rbset.Black then Some (lh + 1) else Some lh
       | _ -> None)
  in
  let rec check_red_property node =
    match node with
    | Rbset.Empty -> true
    | Rbset.Node (Rbset.Red, Rbset.Node (Rbset.Red, _, _, _), _, _)
    | Rbset.Node (Rbset.Red, _, _, Rbset.Node (Rbset.Red, _, _, _)) -> false
    | Rbset.Node (_, left, _, right) ->
      check_red_property left && check_red_property right
  in
  match check_black_height tree with
  | Some _ -> check_red_property tree
  | None -> false
;;

let test_color_property =
  QCheck.Test.make ~name:"tree is correctly colored" ~count:100 arb_tree is_red_black_tree
;;

let run_tests =
  let open Alcotest in
  run
    "Rbset Tests"
    [ "test_empty", [ test_case "test_empty" `Quick test_empty ]
    ; "test_insert", [ test_case "test_insert" `Quick test_insert ]
    ; ( "test_insert_multiple"
      , [ test_case "test_insert_multiple" `Quick test_insert_multiple ] )
    ; "test_to_list", [ test_case "test_to_list" `Quick test_to_list ]
    ; ( "test_insert_duplicates"
      , [ test_case "test_insert_duplicates" `Quick test_insert_duplicates ] )
    ; "test_insert_order", [ test_case "test_insert_order" `Quick test_insert_order ]
    ; ( "test_insert_and_remove"
      , [ test_case "test_insert_and_remove" `Quick test_insert_and_remove ] )
    ; ( "test_insert_and_check_order"
      , [ test_case "test_insert_and_check_order" `Quick test_insert_and_check_order ] )
    ; "test_remove", [ test_case "test_remove" `Quick test_remove ]
    ; ( "test_remove_nonexistent"
      , [ test_case "test_remove_nonexistent" `Quick test_remove_nonexistent ] )
    ; "test_remove_all", [ test_case "test_remove_all" `Quick test_remove_all ]
    ; ( "monoid_properties"
      , [ test_case "test_monoid_identity" `Quick test_monoid_identity
        ; test_case "test_monoid_associativity" `Quick test_monoid_associativity
        ] )
    ; ( "property_based"
      , [ QCheck_alcotest.to_alcotest test_insert_property
        ; QCheck_alcotest.to_alcotest test_remove_property
        ; QCheck_alcotest.to_alcotest test_union_property
        ; QCheck_alcotest.to_alcotest test_operations_property
        ; QCheck_alcotest.to_alcotest test_color_property
        ] )
    ; "test_fold", [ test_case "test_fold" `Quick test_fold ]
    ; "test_map", [ test_case "test_map" `Quick test_map ]
    ; "test_filter", [ test_case "test_filter" `Quick test_filter ]
    ]
;;
