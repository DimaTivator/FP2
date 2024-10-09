open Rbset

let () =
  let open Rbset in
  let set =
    empty |> remove 20 |> insert 10 |> insert 10 |> insert 20 |> insert 15
  in
  Printf.printf "Set contains 15: %b\n" (member 15 set);
  Printf.printf "Set contains 25: %b\n" (member 25 set);
  Printf.printf "Elements in set: %s\n"
    (String.concat ", " (List.map string_of_int (to_list set)))
