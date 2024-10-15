open Rbset
open Stdio

let () =
  let open Rbset in
  let set = empty |> remove 20 |> insert 10 |> insert 10 |> insert 20 |> insert 15 in
  print_endline (Printf.sprintf "Set contains 15: %b" (member 15 set));
  print_endline (Printf.sprintf "Set contains 25: %b" (member 25 set));
  print_endline
    (Printf.sprintf
       "Elements in set: %s"
       (String.concat ", " (List.map string_of_int (to_list set))))
;;
