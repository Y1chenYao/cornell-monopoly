open Yojson.Basic.Util

type ordinary = {
  description : string;
  is_star : bool;
}

type teleport = {
  description : string;
  is_bus : bool;
  dest_loop : int;
  dest_id : int;
}

type event = { description : string }

type rnd_event = {
  description : string;
  brb_change : int;
  gpa_change : float;
  mood_change : int;
}

type gadget = {
  name : string;
  description : string;
  cost : int;
  gpa_change : float;
  mood_change : int;
}

type shop = { shop_gadget : gadget list }

type node_info =
  | Ordinary of ordinary
  | Teleport of teleport
  | Event of event
  | Shop of shop

type node = {
  id : int;
  node_info : node_info;
}

type area = {
  area : string;
  loop : int;
  description : string;
  node_lst : node list;
}

type t = {
  area_lst : area list;
  start_node : int * int;
  event_lst : rnd_event list;
}

let gadget_of_json j =
  {
    name = j |> member "name" |> to_string;
    description = j |> member "description" |> to_string;
    cost = j |> member "cost" |> to_int;
    gpa_change = j |> member "gpa_change" |> to_float;
    mood_change = j |> member "mood_change" |> to_int;
  }

let node_info_of_json (j : Yojson.Basic.t) : node_info =
  if member "ordinary" j != `Null then
    let r = j |> member "ordinary" in
    Ordinary
      {
        description = r |> member "description" |> to_string;
        is_star = r |> member "is_star" |> to_bool;
      }
  else if member "teleport" j != `Null then
    let r = j |> member "teleport" in
    Teleport
      {
        description = r |> member "description" |> to_string;
        is_bus = r |> member "is_bus" |> to_bool;
        dest_loop = r |> member "dest_loop" |> to_int;
        dest_id = r |> member "dest_id" |> to_int;
      }
  else if member "shop" j != `Null then
    let r = j |> member "shop" in
    Shop
      {
        shop_gadget =
          r |> member "gadget" |> to_list |> List.map gadget_of_json;
      }
  else
    let r = j |> member "event" in
    Event { description = r |> member "description" |> to_string }

let node_of_json (j : Yojson.Basic.t) : node =
  {
    id = j |> member "id" |> to_int;
    node_info = j |> member "node_info" |> node_info_of_json;
  }

let area_of_json (j : Yojson.Basic.t) : area =
  {
    area = j |> member "area" |> to_string;
    loop = j |> member "loop" |> to_int;
    description = j |> member "description" |> to_string;
    node_lst = j |> member "nodes" |> to_list |> List.map node_of_json;
  }

let event_of_json (j : Yojson.Basic.t) : rnd_event =
  {
    description = j |> member "description" |> to_string;
    brb_change = j |> member "brb_change" |> to_int;
    gpa_change = j |> member "gpa_change" |> to_float;
    mood_change = j |> member "mood_change" |> to_int;
  }

let areas_of_json (j : Yojson.Basic.t) : t =
  {
    area_lst = j |> member "areas" |> to_list |> List.map area_of_json;
    start_node =
      (let loop = j |> member "start node" |> member "loop" |> to_int in
       let id = j |> member "start node" |> member "id" |> to_int in
       (loop, id));
    event_lst =
      j |> member "rnd events" |> to_list |> List.map event_of_json;
  }

let from_json (json : Yojson.Basic.t) : t =
  try areas_of_json json
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let tup_to_node tup model =
  let x, y = tup in
  let campus = List.nth model.area_lst (x - 1) in
  List.find (fun x -> x.id = y) campus.node_lst

let get_description = function
  | { node_info } -> (
      match node_info with
      | Ordinary { description } -> description
      | Teleport { description } -> description
      | Event { description } -> description
      | Shop _ -> "You entered a shop.")

let rand_node model =
  let a_num = Random.int @@ List.length model.area_lst in
  let n_num =
    (List.nth model.area_lst a_num).node_lst |> List.length
    |> Random.int
  in
  (a_num + 1, n_num + 1)

let rand_event model =
  let e_num = Random.int @@ List.length model.event_lst in
  let rnd_event = List.nth model.event_lst e_num in
  let { description; brb_change; gpa_change; mood_change } =
    rnd_event
  in
  (description, brb_change, gpa_change, mood_change)

let get_node_lst_by_index i m =
  let a_lst = m.area_lst in
  let area = List.nth a_lst (i - 1) in
  area.node_lst

let get_area_name_by_index i m =
  let a_lst = m.area_lst in
  let area = List.nth a_lst (i - 1) in
  area.area

let get_node_index n = n.id

let is_ordinary node =
  match node.node_info with
  | Ordinary _ -> true
  | _ -> false

let is_bus node =
  match node.node_info with
  | Teleport n -> n.is_bus
  | _ -> false

let is_bridge node =
  match node.node_info with
  | Teleport n -> not n.is_bus
  | _ -> false

let is_event node =
  match node.node_info with
  | Event _ -> true
  | _ -> false

let is_shop node =
  match node.node_info with
  | Shop _ -> true
  | _ -> false

let is_teleport node =
  match node.node_info with
  | Teleport _ -> true
  | _ -> false

let get_destination (x, y) model =
  let node_lst = get_node_lst_by_index x model in
  let node = List.nth node_lst (y - 1) in
  match node.node_info with
  | Teleport { dest_loop; dest_id } -> (dest_loop, dest_id)
  | _ -> failwith "unsupported teleport node"

let shop_info node =
  match node.node_info with
  | Shop t -> t.shop_gadget
  | _ -> failwith "not a shop node"

let shop_gadget_names node =
  match node.node_info with
  | Shop t ->
      List.fold_left (fun acc x -> x.name :: acc) [] t.shop_gadget
  | _ -> failwith "not a shop node"

let shop_gadget_names_and_price node =
  match node.node_info with
  | Shop t ->
      List.fold_left
        (fun acc x ->
          (x.name ^ ": " ^ string_of_int x.cost ^ "BRB") :: acc)
        [] t.shop_gadget
  | _ -> failwith "not a shop node"

let get_gadget node name =
  match node.node_info with
  | Shop t -> List.find (fun x -> x.name = name) t.shop_gadget
  | _ -> failwith "not a shop node"

let get_gadget_price name node =
  let g = get_gadget node name in
  g.cost

let get_gadget_mood_influene name node =
  let g = get_gadget node name in
  g.mood_change

let get_gadget_gpa_influence name node =
  let g = get_gadget node name in
  g.gpa_change

let rec star_gen model state =
  let n = tup_to_node (rand_node model) model in
  match n.node_info with
  | Ordinary { description; is_star } ->
      Ordinary { description; is_star = true }
  | _ -> star_gen model state

let bus tup model =
  let n = tup_to_node tup model in
  match n.node_info with
  | Teleport t -> if t.is_bus then (t.dest_loop, t.dest_id) else tup
  | _ -> failwith "Invalid"

let bridge_msg =
  "You reached a bridge. Type 'yes' to cross it. Otherwise, do not \
   cross it."

(* [walk a b] is walk within the length of the loop.*)
let walk a b = if a < b then a + 1 else 1

let rec move_dis_zero n tup res_list model =
  match n.node_info with
  | Teleport t ->
      if t.is_bus then [ bus tup model ] @ res_list else res_list
  | _ -> res_list

let rec move i tup res_list model =
  let x, y = tup in
  let n = tup_to_node (x, y) model in
  if i = 0 then move_dis_zero n tup res_list model
  else
    let campus = List.nth model.area_lst (x - 1) in
    let l = List.length campus.node_lst in
    match n.node_info with
    | Teleport t ->
        if not t.is_bus then (
          print_endline bridge_msg;
          print_string ">";
          match read_line () with
          | str ->
              if String.trim str = "yes" then
                let dest = (t.dest_loop, t.dest_id) in
                if i - 1 = 0 then dest :: res_list
                else
                  move (i - 2)
                    (t.dest_loop, walk t.dest_id l)
                    ([ (t.dest_loop, walk t.dest_id l); dest ]
                    @ res_list)
                    model
              else
                move (i - 1)
                  (x, walk y l)
                  ((x, walk y l) :: res_list)
                  model)
        else
          move (i - 1) (x, walk y l) ((x, walk y l) :: res_list) model
    | _ -> move (i - 1) (x, walk y l) ((x, walk y l) :: res_list) model
