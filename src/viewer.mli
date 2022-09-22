val node_loc_to_board_loc : int * int -> int * int
(** [node_loc_to_board_loc tup] returns the corresponding coordinates in
    the terminal coordinate system *)

val parse_area_name : Model.t -> int -> string * string
(** [parse_area_name m i] returns padded tuple of name of area indexed
    [i] in model [m] *)

val update_and_print_board : Model.t -> State.state -> unit
(** [update_and_print_board m st] print out the board in the terminal
    with model [m] and state [st] *)

val batch_convert_to_node : int -> Model.node list -> (int * int) list
(** [batch_convert_to_node i lst] returns the coordiate tuple list of
    nodes given the area number [i] and the node list [lst] *)

val filter_area_by_tag : Model.t -> int -> string -> Model.node list
(** [filter_area_by_tag m i str] returns the coordiate tuple list of
    nodes in the area number [i] and node type [str] under model [m] *)
