open Model
open State

type node =
  | Ordinary
  | Event
  | Shop
  | Rbus
  | Ybus
  | Bridge
  | Line
  | Name
  | Player
  | Star  (** The abstract type of values representing modes. *)

module BoardTup = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    if x1 > x2 then 1
    else if x1 < x2 then -1
    else if y1 > y2 then 1
    else if y1 < y2 then -1
    else 0
end

module Board = Map.Make (BoardTup)
(** The map for storing node type and string to an integer tuple. *)

exception UnexpectedNodeLoc

let empty_board = Board.empty

let pin_point_first_node node_loc =
  match node_loc with
  | 1, _ -> (2, 4)
  | 2, _ -> (2, 10)
  | 3, _ -> (20, 4)
  | 4, _ -> (20, 10)
  | _ -> raise UnexpectedNodeLoc

let node_loc_to_board_loc (node_loc : int * int) : int * int =
  let p_x, p_y = pin_point_first_node node_loc in
  match node_loc with
  | _, n when n >= 1 && n <= 4 -> (p_x, p_y - n + 1)
  | _, n when n > 4 && n <= 7 -> (p_x + (3 * (n - 4)), p_y - 3)
  | _, n when n > 7 && n <= 10 -> (p_x + 9, p_y + n - 7 - 3)
  | _, n when n > 10 && n <= 12 -> (p_x + 9 - (3 * (n - 10)), p_y)
  | _ -> raise UnexpectedNodeLoc

(* padding *)
let player_name_to_char (str : string) : string =
  String.get str 0 |> Char.uppercase_ascii |> Char.escaped

let pad_name s =
  let len = String.length s in
  if len > 8 then failwith s
  else
    let pad_left = (8 - len) / 2 in
    let pad_right = 8 - len - pad_left in
    String.make pad_left ' ' ^ s ^ String.make pad_right ' '

let repad_name s ori = s ^ "," ^ String.trim ori |> pad_name

(* the following map functions add node type and string to board *)
let map_ordinary lst board =
  let f board (x, y) =
    board
    |> Board.add (x - 1, y) (Ordinary, "*      *")
    |> Board.add (x, y) (Ordinary, "*      *")
    |> Board.add (x + 1, y) (Ordinary, "*      *")
  in
  List.fold_left f board lst

let map_rbus lst board =
  let f board (x, y) =
    board
    |> Board.add (x - 1, y) (Rbus, "  bus   ")
    |> Board.add (x, y) (Rbus, "*      *")
    |> Board.add (x + 1, y) (Rbus, "*      *")
  in
  List.fold_left f board lst

let map_ybus lst board =
  let f board (x, y) =
    board
    |> Board.add (x - 1, y) (Ybus, "  bus   ")
    |> Board.add (x, y) (Ybus, "*      *")
    |> Board.add (x + 1, y) (Ybus, "*      *")
  in
  List.fold_left f board lst

let map_bridge lst board =
  let f board (x, y) =
    board
    |> Board.add (x - 1, y) (Bridge, " bridge ")
    |> Board.add (x, y) (Bridge, "*      *")
    |> Board.add (x + 1, y) (Bridge, "*      *")
  in
  List.fold_left f board lst

let map_shop lst board =
  let f board (x, y) =
    board
    |> Board.add (x - 1, y) (Shop, "  shop  ")
    |> Board.add (x, y) (Shop, "*      *")
    |> Board.add (x + 1, y) (Shop, "*      *")
  in
  List.fold_left f board lst

let map_event lst board =
  let f board (x, y) =
    board
    |> Board.add (x - 1, y) (Event, " event  ")
    |> Board.add (x, y) (Event, "*      *")
    |> Board.add (x + 1, y) (Event, "*      *")
  in
  List.fold_left f board lst

let map_lines lst board =
  let f board tup = board |> Board.add tup (Line, "--------") in
  List.fold_left f board lst

let parse_area_name (model : Model.t) (area_num : int) : string * string
    =
  let str = get_area_name_by_index area_num model in
  let name_lst =
    String.split_on_char ' ' str
    |> List.filter (fun x -> String.length x > 0)
  in
  match name_lst with
  | [ fst; snd ] -> (pad_name fst, pad_name snd)
  | _ -> failwith "unsupported area name"

let map_area_name model board =
  let area_1_fst, area_1_snd = parse_area_name model 1 in
  let area_2_fst, area_2_snd = parse_area_name model 2 in
  let area_3_fst, area_3_snd = parse_area_name model 3 in
  let area_4_fst, area_4_snd = parse_area_name model 4 in
  board
  |> Board.add (5, 2) (Name, area_1_fst)
  |> Board.add (5, 3) (Name, area_1_snd)
  |> Board.add (5, 8) (Name, area_2_fst)
  |> Board.add (5, 9) (Name, area_2_snd)
  |> Board.add (23, 2) (Name, area_3_fst)
  |> Board.add (23, 3) (Name, area_3_snd)
  |> Board.add (23, 8) (Name, area_4_fst)
  |> Board.add (23, 9) (Name, area_4_snd)

let map_names model board =
  map_area_name model board
  |> Board.add (14, 4) (Name, " Player ")
  |> Board.add (14, 5) (Name, "  GPA   ")
  |> Board.add (14, 6) (Name, "  BRB   ")
  |> Board.add (14, 7) (Name, "  Mood  ")

let batch_convert_to_node (x : int) (node_lst : Model.node list) =
  let y_lst = List.map get_node_index node_lst in
  List.map (fun y -> (x, y)) y_lst

let filter_area_by_tag (model : Model.t) (x : int) (s : string) =
  let node_lst = get_node_lst_by_index x model in
  match s with
  | "ordinary" -> List.filter is_ordinary node_lst
  | "event" -> List.filter is_event node_lst
  | "shop" -> List.filter is_shop node_lst
  | "bridge" -> List.filter is_bridge node_lst
  | "bus" -> List.filter is_bus node_lst
  | _ -> failwith "unsupported node type"

let rec filter_areas_by_tag
    (model : Model.t)
    (area_num : int)
    (s : string) =
  if area_num = 0 then []
  else
    (filter_area_by_tag model area_num s
    |> batch_convert_to_node area_num)
    @ filter_areas_by_tag model (area_num - 1) s

let map_buses (model : Model.t) board =
  let indices = filter_areas_by_tag model 4 "bus" in
  let node_r_fst = List.hd indices in
  let node_r_snd = get_destination node_r_fst model in
  let remaining =
    List.filter (fun x -> x <> node_r_fst && x <> node_r_snd) indices
    |> List.map node_loc_to_board_loc
  in
  map_rbus
    [
      node_r_fst |> node_loc_to_board_loc;
      node_r_snd |> node_loc_to_board_loc;
    ]
    board
  |> map_ybus remaining

let lines = [ (8, 5); (8, 6); (23, 5); (23, 6) ]

(* add the entire model of nodes to the board *)
let input_from_model (model : Model.t) =
  empty_board
  |> map_ordinary
       (filter_areas_by_tag model 4 "ordinary"
       |> List.map node_loc_to_board_loc)
  |> map_bridge
       (filter_areas_by_tag model 4 "bridge"
       |> List.map node_loc_to_board_loc)
  |> map_event
       (filter_areas_by_tag model 4 "event"
       |> List.map node_loc_to_board_loc)
  |> map_shop
       (filter_areas_by_tag model 4 "shop"
       |> List.map node_loc_to_board_loc)
  |> map_buses model |> map_lines lines |> map_names model

(* print option *)
let white_print = ANSITerminal.(print_string [ on_white; black ])
let yellow_print = ANSITerminal.(print_string [ on_yellow; black ])
let red_print = ANSITerminal.(print_string [ on_red; white ])
let green_print = ANSITerminal.(print_string [ on_green; black ])
let cyan_print = ANSITerminal.(print_string [ on_cyan; black ])
let magenta_print = ANSITerminal.(print_string [ on_magenta; black ])
let green_text_print = ANSITerminal.(print_string [ green; Bold ])
let white_text_print = ANSITerminal.(print_string [ white; Bold ])
let player_print = ANSITerminal.(print_string [ on_black; white; Bold ])

let star_print =
  ANSITerminal.(print_string [ on_magenta; yellow; Bold ])

(* print the nodes according to node type *)
let palette_all tup board : unit =
  match Board.find tup board with
  | Ordinary, s ->
      magenta_print s;
      print_string " "
  | Event, s ->
      cyan_print s;
      print_string " "
  | Shop, s ->
      white_print s;
      print_string " "
  | Rbus, s ->
      red_print s;
      print_string " "
  | Ybus, s ->
      yellow_print s;
      print_string " "
  | Bridge, s ->
      green_print s;
      print_string " "
  | Line, s ->
      green_text_print s;
      print_string " "
  | Name, s ->
      white_text_print s;
      print_string " "
  | Player, s ->
      white_text_print s;
      print_string " "
  | _ -> raise UnexpectedNodeLoc

(* update the board with players *)
let put_players st cur_board =
  let players = get_players_list st in
  let f board player =
    let player_loc = get_location player |> node_loc_to_board_loc in
    let player_cap = get_name player |> player_name_to_char in
    if Board.mem player_loc board then
      let _, ori = Board.find player_loc board in
      if String.(ori |> trim |> length) = 8 then
        Board.update player_loc
          (fun _ -> Some (Player, player_cap |> pad_name))
          board
      else
        Board.update player_loc
          (fun _ -> Some (Player, repad_name player_cap ori))
          board
    else Board.add player_loc (Player, player_cap |> pad_name) board
  in
  List.fold_left f cur_board (List.rev players)

let palatte_player tup board : unit =
  let _, s = Board.find tup board in
  player_print s;
  print_string " "

(* update the board with a star *)
let put_star st cur_board =
  try
    let x, y = extract_star st |> node_loc_to_board_loc in
    if Board.mem (x + 1, y) cur_board then
      Board.update
        (x + 1, y)
        (fun _ -> Some (Star, "  star  "))
        cur_board
    else raise UnexpectedNodeLoc
  with UnexpectedStar -> cur_board

let palette_star tup board : unit =
  let _, s = Board.find tup board in
  star_print s;
  print_string " "

(* update the board with a leaderboard *)
let rec put_stats players (x, y) cur_board =
  match players with
  | player :: t ->
      let new_board =
        begin
          cur_board
          |> Board.add (x, y) (Name, player |> get_name |> pad_name)
          |> Board.add
               (x, y + 1)
               (Name, player |> get_gpa |> string_of_float |> pad_name)
          |> Board.add
               (x, y + 2)
               (Name, player |> get_brb |> string_of_int |> pad_name)
          |> Board.add
               (x, y + 3)
               (Name, player |> get_mood |> string_of_int |> pad_name)
        end
      in
      put_stats t (x + 1, y) new_board
  | [] -> cur_board

(* function and helper functions for the final printing *)
let print_cell (x : int) (y : int) (cur_board : (node * string) Board.t)
    =
  if Board.mem (x, y) cur_board then (
    try palette_all (x, y) cur_board
    with UnexpectedNodeLoc ->
      let _, s = Board.find (x, y) cur_board in
      white_text_print s;
      print_string " ")
  else print_string "         "

let print_board_without_star
    (players : (int * int) list)
    (cur_board : (node * string) Board.t) =
  for x = 1 to 30 do
    for y = 1 to 10 do
      if List.mem (x, y) players then palatte_player (x, y) cur_board
      else print_cell x y cur_board
    done;
    print_newline ();
    if (not (List.mem x [ 12; 15; 18 ])) && x mod 3 = 0 then
      print_newline ()
  done

let print_board_with_star
    (star_x : int)
    (star_y : int)
    (players : (int * int) list)
    (cur_board : (node * string) Board.t) =
  for x = 1 to 30 do
    for y = 1 to 10 do
      if List.mem (x, y) players then palatte_player (x, y) cur_board
      else if (x, y) = (star_x + 1, star_y) then
        palette_star (x, y) cur_board
      else print_cell x y cur_board
    done;
    print_newline ();
    if (not (List.mem x [ 12; 15; 18 ])) && x mod 3 = 0 then
      print_newline ()
  done

let print_board st (cur_board : (node * string) Board.t) =
  try
    let f player = get_location player |> node_loc_to_board_loc in
    let star_x, star_y = extract_star st |> node_loc_to_board_loc in
    let players = List.map f (get_players_list st) in
    print_board_with_star star_x star_y players cur_board
  with UnexpectedStar ->
    let f player = get_location player |> node_loc_to_board_loc in
    let players = List.map f (get_players_list st) in
    print_board_without_star players cur_board

let update_and_print_board (m : Model.t) (st : state) =
  input_from_model m |> put_players st |> put_star st
  |> put_stats (get_players_list st) (15, 4)
  |> print_board st
