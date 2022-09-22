exception Empty
exception Malformed

type command =
  | Roll
  | Buy of string list
  | Use of string list
  | Pass
  | Quit
  | Inventory

let parse str =
  if String.length str = 0 then raise Empty
  else
    let command_lst =
      String.split_on_char ' ' str
      |> List.filter (fun x -> String.length x > 0)
    in
    match command_lst with
    | [ "roll" ] -> Roll
    | [ "quit" ] -> Quit
    | s :: t when s = "buy" -> Buy t
    | s :: t when s = "use" -> Use t
    | [ "pass" ] -> Pass
    | [ "inventory" ] -> Inventory
    | _ -> raise Malformed
