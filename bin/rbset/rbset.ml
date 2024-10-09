module Rbset = struct
  type color = Red | Black
  type 'a tree = Empty | Node of color * 'a tree * 'a * 'a tree

  let empty = Empty

  let rec member x = function
    | Empty -> false
    | Node (_, left, y, right) ->
        if x < y then member x left else if x > y then member x right else true

  let balance = function
    | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
    | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
    | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
    | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
        Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | color, a, x, b -> Node (color, a, x, b)

  let insert x s =
    let rec ins = function
      | Empty -> Node (Red, Empty, x, Empty)
      | Node (color, a, y, b) as s ->
          if x < y then balance (color, ins a, y, b)
          else if x > y then balance (color, a, y, ins b)
          else s
    in
    match ins s with
    | Node (_, a, y, b) -> Node (Black, a, y, b)
    | Empty -> failwith "RBTreeSet: insert failed"

  let remove x s =
    let rec remove_min = function
      | Empty -> failwith "RBTreeSet: remove_min failed"
      | Node (_, Empty, y, right) -> (y, right)
      | Node (color, left, y, right) ->
          let min, new_left = remove_min left in
          (min, balance (color, new_left, y, right))
    in
    let rec del = function
      | Empty -> Empty
      | Node (color, left, y, right) -> (
          if x < y then balance (color, del left, y, right)
          else if x > y then balance (color, left, y, del right)
          else
            match right with
            | Empty -> left
            | _ ->
                let min, new_right = remove_min right in
                balance (color, left, min, new_right))
    in
    match del s with
    | Node (_, a, y, b) -> Node (Black, a, y, b)
    | Empty -> Empty

  let rec to_list = function
    | Empty -> []
    | Node (_, left, x, right) -> to_list left @ (x :: to_list right)
end
