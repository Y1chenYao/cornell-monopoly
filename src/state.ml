open Model
open Command

type player = {
  name : string;
  brb : int;
  gpa : float;
  mood : int;
  location : int * int;
  gadget : string list;
}

type state = {
  current_player : int;
  current_turn : int;
  players_list : player list;
  star : (int * int) option ref;
}

let init_player name =
  {
    name;
    brb = 10;
    gpa = 3.0;
    mood = 10;
    location = (1, 4);
    gadget = [];
  }

let init_state lst =
  {
    current_player = 0;
    current_turn = 0;
    players_list = lst;
    star = ref None;
  }

exception UnexpectedStar

(* printline shortcut *)
let simple_print = ANSITerminal.print_string [ ANSITerminal.white ]
let fancy_print = ANSITerminal.print_string [ ANSITerminal.green ]
let danger_print = ANSITerminal.print_string [ ANSITerminal.red ]

(* messages *)

let shop_welcome_msg =
  "You have reached a shop. Here are the items in the shop and their \
   prices:"

let pass_shop_msg =
  "You have passed this shop. Come back soon and see you later!"

let cannot_roll_msg =
  "cannot roll right now in buy state. Hint: did you want to pass?"

let cannot_buy_msg =
  "You do not have enough BRB to buy at this moment; Please buy a\n\
  \   gadget that cost less BRB, or choose to pass this shop."

let low_mood_msg =
  ", you are too depressed to continue playing this game! Plz take a \
   rest!"

let invalid_msg =
  {|
  Not a valid command. Possible command:
  
      buy <item>   --if you want to purchase an item with brb
      use <item>   --if you want to use an item within your inventory
      inventory    --if you want to check your inventory
      pass         --if you want to leave the shop
      quit         --Warning: terminate the game
  |}

let quit_msg = "You entered quit. Goodbye!"
let gatget_prompt = "Do you want to buy a gadget or use a gadget?"
let inventory_prompt = "Here is your inventory: "

let extract_star state =
  match !(state.star) with
  | Some node -> node
  | None -> raise UnexpectedStar

(** [generate_next_star_loc] generate a new location for star if after
    certain step. Else, return none*)
let rec generate_next_star_loc state model =
  Random.self_init ();
  if state.current_turn mod 4 = 0 then
    let loop = Random.int 3 + 1 in
    let node = Random.int 11 + 1 in
    let possible_loc = Model.tup_to_node (loop, node) model in
    if
      Model.is_event possible_loc
      || Model.is_shop possible_loc
      || Model.is_bus possible_loc
      || Model.is_bridge possible_loc
    then generate_next_star_loc state model
    else Some (loop, node)
  else Some (extract_star state)

let get_players_list state = state.players_list
let get_name player = player.name
let get_location player = player.location
let get_brb player = player.brb
let get_gpa player = player.gpa
let get_mood player = player.mood
let get_current_player state = state.current_player
let change_name inputed_name p = { p with name = inputed_name }

let check_winning state : string list =
  List.fold_left
    (fun acc x -> if x.gpa >= 4.3 then x.name :: acc else acc)
    [] state.players_list

let check_mood state =
  let p_lst = state.players_list in
  let cur_p_index = state.current_player in
  let cur_p = List.nth p_lst cur_p_index in
  if get_mood cur_p < 0 then (
    let name = get_name cur_p in
    print_endline (name ^ low_mood_msg);
    let new_p_index =
      if cur_p_index = List.length p_lst - 1 then 0 else cur_p_index
    in
    {
      state with
      current_player = new_p_index;
      players_list = List.filter (fun x -> x <> cur_p) p_lst;
    })
  else state

let simple_update state new_player_list =
  { state with players_list = new_player_list }

(** [change_turn st] change the turn of st. *)
let change_turn state =
  if state.current_player = 0 then state.current_turn + 1
  else state.current_turn

(** [change_player st] change the state to reflect current player st. *)
let change_player state =
  if state.current_player = List.length state.players_list - 1 then 0
  else state.current_player + 1

let event_influence_player
    (player : player)
    (event : string * int * float * int) =
  match event with
  | d, b, g, m ->
      print_endline d;
      {
        player with
        brb = player.brb + b;
        gpa = player.gpa +. g;
        mood = player.mood + m;
      }

(** [item_exist p i] returns true if i exists in p. False otherwise. *)
let item_exist player item_name =
  List.exists (fun x -> x = item_name) player.gadget

(**[covert_item_name s] converts a string list s of Buy into a string
   with space in between. No trailing white space.*)
let convert_item_name s =
  List.fold_left (fun acc x -> x ^ " " ^ acc) "" s |> String.trim

(** [print_welcome_msg f n] pribnts out the welcome message if f is
    true. *)
let print_welcome_msg first_time cur_node =
  if first_time then print_endline shop_welcome_msg;
  List.iter print_endline (shop_gadget_names_and_price cur_node)

let get_inventory player =
  if player.gadget <> [] then (
    fancy_print inventory_prompt;
    List.iter print_endline player.gadget)
  else danger_print "Your inventory is empty!\n"

(** [shop_influence_player b p n] prompts the player for the items in
    the current shop. It prints out the items in the shop and the user
    can add the item to the inventory if they have enough brb. *)
let rec shop_influence_player first_time player cur_node : player =
  print_welcome_msg first_time cur_node;
  print_endline gatget_prompt;
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline invalid_msg;
      shop_influence_player false player cur_node
  | a_message -> (
      try
        let a_parsed_command = parse a_message in
        match_player_cmd a_parsed_command player cur_node
      with failure ->
        print_endline invalid_msg;
        shop_influence_player false player cur_node)

and match_player_cmd a_parsed_command player cur_node =
  match a_parsed_command with
  | Quit ->
      print_endline quit_msg;
      Stdlib.exit 0
  | Buy s -> buy_item s player cur_node
  | Use s -> use_item player (convert_item_name s) cur_node
  | Pass ->
      print_endline pass_shop_msg;
      player
  | Inventory ->
      get_inventory player;
      shop_influence_player false player cur_node
  | Roll ->
      print_endline cannot_roll_msg;
      shop_influence_player false player cur_node

and use_item player item_name cur_node =
  if item_exist player item_name then (
    print_endline ("Cool! You have used " ^ item_name);
    {
      player with
      gpa = player.gpa +. get_gadget_gpa_influence item_name cur_node;
      mood = player.mood + get_gadget_mood_influene item_name cur_node;
      gadget = List.filter (fun x -> x <> item_name) player.gadget;
    })
  else (
    print_endline "You do not have this item yet.";
    player)

and buy_item s player cur_node =
  let buy_item = convert_item_name s in
  if
    player.brb >= get_gadget_price buy_item cur_node
    && List.mem buy_item player.gadget = false
  then (
    print_endline ("You bought " ^ buy_item ^ ".");
    shop_influence_player false
      {
        player with
        brb = player.brb - get_gadget_price buy_item cur_node;
        gadget = [ buy_item ] @ player.gadget;
      }
      cur_node)
  else if List.mem buy_item player.gadget = true then (
    print_endline ("You have " ^ buy_item ^ " already. \n\n");
    shop_influence_player false player cur_node)
  else (
    print_endline cannot_buy_msg;
    player)

(** [star_on_path p s] determined if there are any star from the
    player's beginning location to their ending position. If there exist
    star on path rteturn 0.3 ; else return 0. *)
let star_on_path path state =
  match !(state.star) with
  | Some star_loc -> if List.mem star_loc path then 2 else 0
  | _ -> 0

let update_player path state (model : Model.t) =
  let player = List.nth state.players_list state.current_player in
  let up =
    {
      player with
      brb = player.brb + star_on_path path state;
      location = List.hd path;
    }
  in
  let cur_node = Model.tup_to_node up.location model in
  let new_up =
    if Model.is_event cur_node then
      let event = Model.rand_event model in
      event_influence_player up event
    else if Model.is_shop cur_node then
      shop_influence_player true up cur_node
    else up
  in
  List.mapi
    (fun i x -> if i = state.current_player then new_up else x)
    state.players_list

let step path state (model : Model.t) =
  {
    current_player = change_player state;
    current_turn = change_turn state;
    players_list = update_player path state model;
    star = ref (generate_next_star_loc state model);
  }