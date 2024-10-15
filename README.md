## RB-tree set
### Андриянов Дмитрий P3312

*Rbset* - полиморфная структура, реализующая интерфейс set 

Функция *member* проверяет, лежит ли переданное значение в дереве
```ocaml
let rec member x = function
    | Empty -> false
    | Node (_, left, value, right) ->
      if x < value then member x left else if x > value then member x right else true
  ;;
```

Функция *balance* возвращает свойства сбалансированности дереву, если они где-то нарушены
```ocaml
 let balance = function
    (* Case 1: Left-Left Red violation *)
    | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
    (* Case 2: Left-Right Red violation *)
    | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
    (* Case 3: Right-Left Red violation *)
    | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
    (* Case 4: Right-Right Red violation *)
    | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
      (* Rebalance by rotating and recoloring *)
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    (* No violation, return the node as is *)
    | color, a, x, b -> Node (color, a, x, b)
  ;;
```

Функция *insert* вставляет новый ключ в дерево. Вставка происходит как в BST, добавляется только перебалансировка
```ocaml
let insert value tree =
    let rec ins = function
      | Empty -> Node (Red, Empty, value, Empty)
      | Node (color, left, x, right) as tree ->
        if value < x
        then balance (color, ins left, x, right)
        else if value > x
        then balance (color, left, x, ins right)
        else tree
    in
    match ins tree with
    | Node (_, left, value, right) -> Node (Black, left, value, right)
    | Empty -> failwith "RBTreeSet: insert failed"
  ;;
```

Функция *remove* удаляет переданный ключ из дерева. Удаление как в BST:
1) Находим требуемый ключ 
2) Находим первый больший этого ключа элемент и отрезаем его (это лист)
3) Ставим его на место удаляемого ключа
Аналогично *insert* везде добавляем перебалансировки
```ocaml
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
        if value < x
        then balance (color, del left, x, right)
        else if value > x
        then balance (color, left, x, del right)
        else (
          match right with
          | Empty -> left
          | _ ->
            let min, new_right = find_min right in
            balance (color, left, min, new_right))
    in
    match del tree with
    | Node (_, left, value, right) -> Node (Black, left, value, right)
    | Empty -> Empty
  ;;
```

Функция *union* сливает 2 дерева в одно
```ocaml
 let rec union t1 t2 =
    match t1, t2 with
    | Empty, t | t, Empty -> t
    | Node (_, left1, value1, right1), _ ->
      let t1' = union left1 (union right1 t2) in
      insert value1 t1'
  ;;
```

Функция *filter* принимает предикат и фильструет ключи дерева, в зависимости от него
```ocaml
let rec filter f = function
    | Empty -> Empty
    | Node (color, left, value, right) ->
      let left' = filter f left in
      let right' = filter f right in
      if f value then balance (color, left', value, right') else union left' right'
  ;;
```

Функция *map* принимает функцию и применяет ее ко всем ключам дерева
```ocaml
let rec map f = function
    | Empty -> Empty
    | Node (_, left, value, right) ->
      let left' = map f left in
      let right' = map f right in
      insert (f value) (union left' right')
  ;;
```

Функция *fold* обходит дерево и делает свертку с заданной функцией 
```ocaml
  let rec fold f acc = function
    | Empty -> acc
    | Node (_, left, value, right) ->
      let acc' = fold f acc left in
      let acc'' = f acc' value in
      fold f acc'' right
  ;;
```

