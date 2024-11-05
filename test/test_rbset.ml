open Rbset
module IntSet = Rbset (Int)
module StrSet = Rbset (String)
module StandardIntSet = Set.Make (Int)
module StandardStrSet = Set.Make (String)

let test_strset_insert =
  let open Alcotest in
  test_case "StrSet insert" `Quick (fun () ->
    let set = StrSet.empty in
    let set = StrSet.insert "hello" set in
    let set = StrSet.insert "world" set in
    let set = StrSet.insert "ocaml" set in
    check (list string) "same elements" [ "hello"; "ocaml"; "world" ] (StrSet.to_list set))
;;

let test_strset_remove =
  let open Alcotest in
  test_case "StrSet remove" `Quick (fun () ->
    let set = StrSet.empty in
    let set = StrSet.insert "hello" set in
    let set = StrSet.insert "world" set in
    let set = StrSet.insert "ocaml" set in
    let set = StrSet.remove "world" set in
    check (list string) "same elements" [ "hello"; "ocaml" ] (StrSet.to_list set))
;;

let arb_int_tree =
  let open QCheck in
  let rec gen_tree n =
    match n with
    | 0 -> Gen.return IntSet.empty
    | _ ->
      Gen.frequency
        [ 1, Gen.return IntSet.empty
        ; 3, Gen.map2 (fun x t -> IntSet.insert x t) Gen.int (gen_tree (n - 1))
        ]
  in
  make ~print:(fun tree -> Print.(list int) (IntSet.to_list tree)) (gen_tree 10)
;;

let arb_operations =
  let open QCheck in
  let gen_op =
    Gen.oneof
      [ Gen.map (fun x -> `Insert x) Gen.int; Gen.map (fun x -> `Remove x) Gen.int ]
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
             | `Remove x -> "Remove " ^ string_of_int x)
           ops))
    (gen_ops 100)
;;

let apply_operations ops rbset intset =
  let rec aux ops rbset intset =
    match ops with
    | [] -> rbset, intset
    | `Insert x :: rest -> aux rest (IntSet.insert x rbset) (StandardIntSet.add x intset)
    | `Remove x :: rest ->
      aux rest (IntSet.remove x rbset) (StandardIntSet.remove x intset)
  in
  aux ops rbset intset
;;

let test_operations_property =
  QCheck.Test.make
    ~name:"operations produce same result as built-in set"
    arb_operations
    (fun ops ->
       let rbset, intset = apply_operations ops IntSet.empty StandardIntSet.empty in
       IntSet.to_list rbset = StandardIntSet.elements intset)
;;

(* Monoid properties *)
let test_monoid_identity =
  QCheck.Test.make
    ~name:"union with empty is identity"
    ~count:10
    arb_int_tree
    (fun tree ->
       let union_with_empty = IntSet.union tree IntSet.empty in
       let union_empty_with = IntSet.union IntSet.empty tree in
       IntSet.equal tree union_with_empty
       && IntSet.equal tree union_empty_with)
;;

let test_monoid_associativity =
  QCheck.Test.make
    ~name:"union is associative"
    ~count:10
    (QCheck.triple arb_int_tree arb_int_tree arb_int_tree)
    (fun (t1, t2, t3) ->
       let union1 = IntSet.union t1 (IntSet.union t2 t3) in
       let union2 = IntSet.union (IntSet.union t1 t2) t3 in
       IntSet.equal union1 union2)
;;

let is_red_black_tree tree =
  let rec check_black_height node =
    match node with
    | IntSet.Empty -> Some 0
    | IntSet.Node (color, left, _, right) ->
      let left_height = check_black_height left in
      let right_height = check_black_height right in
      (match left_height, right_height with
       | Some lh, Some rh when lh = rh ->
         if color = IntSet.Black then Some (lh + 1) else Some lh
       | _ -> None)
  in
  let rec check_red_property node =
    match node with
    | IntSet.Empty -> true
    | IntSet.Node (IntSet.Red, IntSet.Node (IntSet.Red, _, _, _), _, _)
    | IntSet.Node (IntSet.Red, _, _, IntSet.Node (IntSet.Red, _, _, _)) -> false
    | IntSet.Node (_, left, _, right) ->
      check_red_property left && check_red_property right
  in
  match check_black_height tree with
  | Some _ -> check_red_property tree
  | None -> false
;;

let test_color_property =
  QCheck.Test.make
    ~name:"tree is correctly colored"
    ~count:10
    arb_int_tree
    is_red_black_tree
;;

let run_tests =
  let open Alcotest in
  run
    "Rbset Tests"
    [ ( "property_based"
      , [ QCheck_alcotest.to_alcotest test_operations_property
        ; QCheck_alcotest.to_alcotest test_monoid_identity
        ; QCheck_alcotest.to_alcotest test_monoid_associativity
        ; QCheck_alcotest.to_alcotest test_color_property
        ] )
    ; "unit_tests", [ test_strset_insert; test_strset_remove ]
    ]
;;
