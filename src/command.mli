exception Empty
exception Malformed

type command =
  | Roll
  | Buy of string list
  | Use of string list
  | Pass
  | Quit
  | Inventory  
(** The type of values representing commands. *)

val parse : string -> command
(** [parse s] returns a command parsed from string [s] *)
