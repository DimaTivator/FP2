module Rbset (Ord : sig
    type t

    val compare : t -> t -> int
  end) =
struct
  type color =
    | Red
    | Black

  type tree =
    | Empty
    | Node of color * tree * Ord.t * tree

  let empty = Empty

  let rec member x = function
    | Empty -> false
    | Node (_, left, value, right) ->
      (match Ord.compare x value with
       | cmp when cmp < 0 -> member x left
       | cmp when cmp > 0 -> member x right
       | _ -> true)
  ;;

  let balance = function
    | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
    | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
    | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
    | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | color, a, x, b -> Node (color, a, x, b)
  ;;

  let insert value tree =
    let rec ins = function
      | Empty -> Node (Red, Empty, value, Empty)
      | Node (color, left, x, right) as tree ->
        (match Ord.compare value x with
         | cmp when cmp < 0 -> balance (color, ins left, x, right)
         | cmp when cmp > 0 -> balance (color, left, x, ins right)
         | _ -> tree)
    in
    match ins tree with
    | Node (_, left, value, right) -> Node (Black, left, value, right)
    | Empty -> failwith "RBTreeSet: insert failed"
  ;;

  let remove value tree =
    let rec find_min = function
      | Empty -> failwith "RBTreeSet: find_min failed"
      | Node (_, Empty, value, right) -> value, right
      | Node (color, left, value, right) ->
        let min, new_left = find_min left in
        min, balance (color, new_left, value, right)
    in
    let rec del = function
      | Empty -> Empty
      | Node (color, left, x, right) ->
        (match Ord.compare value x with
         | cmp when cmp < 0 -> balance (color, del left, x, right)
         | cmp when cmp > 0 -> balance (color, left, x, del right)
         | _ ->
           (match right with
            | Empty -> left
            | _ ->
              let min, new_right = find_min right in
              balance (color, left, min, new_right)))
    in
    match del tree with
    | Node (_, left, value, right) -> Node (Black, left, value, right)
    | Empty -> Empty
  ;;

  let rec to_list = function
    | Empty -> []
    | Node (_, left, value, right) -> to_list left @ (value :: to_list right)
  ;;

  let rec fold f acc = function
    | Empty -> acc
    | Node (_, left, value, right) ->
      let acc' = fold f acc left in
      let acc'' = f acc' value in
      fold f acc'' right
  ;;

  let rec union t1 t2 =
    match t1, t2 with
    | Empty, t | t, Empty -> t
    | Node (_, left1, value1, right1), _ ->
      let t1' = union left1 (union right1 t2) in
      insert value1 t1'
  ;;

  let rec filter f = function
    | Empty -> Empty
    | Node (color, left, value, right) ->
      let left' = filter f left in
      let right' = filter f right in
      if f value then balance (color, left', value, right') else union left' right'
  ;;

  let rec map f = function
    | Empty -> Empty
    | Node (_, left, value, right) ->
      let left' = map f left in
      let right' = map f right in
      insert (f value) (union left' right')
  ;;

  module Iterator = struct
    type iterator = tree list

    let rec leftmost path = function
      | Empty -> path
      | Node (_, left, value, right) ->
        leftmost (Node (Black, Empty, value, right) :: path) left
    ;;

    (* Start from minimum *)
    let init tree = leftmost [] tree

    (* Next element in in-order traversal *)
    let next = function
      | [] -> None, []
      | Node (_, _, value, right) :: rest ->
        let new_stack = leftmost rest right in
        Some value, new_stack
      | _ -> failwith "Unexpected pattern"
    ;;
  end

  let equal t1 t2 =
    let rec loop iter1 iter2 =
      match Iterator.next iter1, Iterator.next iter2 with
      | (None, _), (None, _) -> true
      | (Some v1, iter1'), (Some v2, iter2') when Ord.compare v1 v2 = 0 ->
        loop iter1' iter2'
      | _ -> false
    in
    loop (Iterator.init t1) (Iterator.init t2)
  ;;
end
