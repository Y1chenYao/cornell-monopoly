type node
(** The abstract type of values representing nodes. *)

type t
(** The abstract type of values representing models. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the model that [j] represents. Requires: [j] is a
    valid JSON model representation. *)

val tup_to_node : int * int -> t -> node
(** [tup_to_node tup model] is the node that [tup] represents. Requires:
    the former integer in the tuple is the loop number, and the latter
    integer is the index in the loop *)

val get_description : node -> string
(** [get_description n] returns the description of node [n] *)

val shop_gadget_names : node -> string list
(** [shop_gadget_names n] returns a string list of all the gadget names. *)

val shop_gadget_names_and_price : node -> string list
(** [shop_gadget_names_and_price n] returns a string list of all the
    gadget names with their respective prices. *)

val get_gadget_price : string -> node -> int
(** [get_gadget_price s n] returns the price of a given gadget. *)

val get_gadget_mood_influene : string -> node -> int
(** [get_gadget_mood_influene s n] returns the influence of mood of a
    given gadget. *)

val get_gadget_gpa_influence : string -> node -> float
(** [get_gadget_gpa_influence s n] returns the influence of gpa of a
    given gadget. *)

val rand_node : t -> int * int
(** [rand_node model] generate a random node from a given model. Return:
    a node in a tuple where the former integer is the loop number, and
    the latter integer is the index in the loop*)

val rand_event : t -> string * int * float * int
(** [rand_node model] generate a random event from a given model.
    Return: an tuple representation event with
    [(description, brb, gpa, mood)] *)

val get_node_lst_by_index : int -> t -> node list
(** [get_node_lst_by_index i m] returns the node list under area indexed
    by [i] of model [m] *)

val get_area_name_by_index : int -> t -> string
(** [get_node_lst_by_index i m] returns the name of area indexed by [i]
    of model [m] *)

val get_node_index : node -> int
(** [get_node_index n t] get the node index *)

val get_destination : int * int -> t -> int * int
(** [get_destination i t] return the deestination as a tup. *)

val is_ordinary : node -> bool
(** [is_ordinary] return true if n is an ordinary node. False otherwise. *)

val is_bus : node -> bool
(** [is_bus] return true if n is an bus node. False otherwise.*)

val is_bridge : node -> bool
(** [is_bridge] return true if n is an bridge node. False otherwise.*)

val is_event : node -> bool
(** [is_event n] returns the boolean value of if the node is an event
    node *)

val is_shop : node -> bool
(** [is_shop n] returns the boolean value of if the node is a shop node*)

val is_teleport : node -> bool
(** [is_teleport n] returns the boolean value of if the node is a
    teleport node*)

val move : int -> int * int -> (int * int) list -> t -> (int * int) list
(** [move i tup model] generate a tuple representing the corresponding
    node from a given model after [i] steps. Return: a node in a tuple
    where the former integer is the loop number, and the latter integer
    is the index in the loop*)

val get_gadget_price : string -> node -> int
(** [get_gadget_price s n] returns the price of gadget s. Raises failure
    if s is not a shop node.*)
