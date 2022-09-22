type player = {
  name : string;
  brb : int;
  gpa : float;
  mood : int;
  location : int * int;
  gadget : string list;
}
(** The type of values representing players. *)

type state
(** The abstract type of values representing the game state. *)

exception UnexpectedStar

val init_player : string -> player
(** [init_player] initialize a new player with name name*)

val init_state : player list -> state
(** [init_state] initializes the state of player at the beginning of the
    game*)

val get_players_list : state -> player list
(** [get_players_list] returns the list of players of the state *)

val get_name : player -> string
(** [get_name p] returns the name of the player [p].*)

val get_location : player -> int * int
(** [get_location p] returns the location of the player [p].*)

val get_brb : player -> int
(** [get_brb p] returns the brb of the player [p].*)

val get_gpa : player -> float
(** [get_gpa p] returns the gpa of the player [p].*)

val get_mood : player -> int
(** [get_mood p] returns the mod of the player [p].*)

val change_turn : state -> int
(** [change_turn state] adds 1 to current turn if all players end their
    turn once, no change otherwise *)

val get_current_player : state -> int
(** [get_current_player state] returns the current player index of the
    state *)

val simple_update : state -> player list -> state
(** [simple_update state] changes the player list to the new player list *)

val change_player : state -> int
(** [change_player state] changes the current player to the next
    acccording to the player list *)

val update_player : (int * int) list -> state -> Model.t -> player list
(** [move i str tup model] updates the player after moving [i] steps in
    the playerlist *)

val step : (int * int) list -> state -> Model.t -> state
(** [step path state str model] updates the current state after a
    player's turn *)

val extract_star : state -> int * int
(** [extract_star state] get the current state after a player's turn *)

val change_name : string -> player -> player
(** [change_name s p] changes the name of p to s *)

val item_exist : player -> string -> bool
(** [item_exist p s] returns true if player p has the item s.*)

val event_influence_player :
  player -> string * int * float * int -> player
(** [event_influence_player p e] returns the updated player with [e]
    signifying the name of the event, brb change, gpa change, and mood
    change on the original player [p] *)

val check_winning : state -> string list
(** [check_winning st] returns a string list if at least one player has
    gpa >= 4.3. Else returns an empty list*)

val check_mood : state -> state
(** [check_mood st] returns a new state where the current player is
    removed if the mood is below 0 *)

val get_inventory : player -> unit
(** [get_inventory p] prints out the current inventory of p.*)