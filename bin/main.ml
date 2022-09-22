open Game
open Viewer
open State
open Model
open Command

exception InvalidName
exception InvalidNumPlayer

(* messages *)
let cmd_err_msg = "This command is not available at this stage. "

let winning_ascii =
  {| 
__    __                                             
/\ \  /\ \                              __            
\ `\`\\/'/ ___   __  __      __  __  __/\_\    ___    
 `\ `\ /' / __`\/\ \/\ \    /\ \/\ \/\ \/\ \ /' _ `\  
   `\ \ \/\ \L\ \ \ \_\ \   \ \ \_/ \_/ \ \ \/\ \/\ \ 
     \ \_\ \____/\ \____/    \ \___x___/'\ \_\ \_\ \_\
      \/_/\/___/  \/___/      \/__//__/   \/_/\/_/\/_/
                                                      
                                                       |}

let invalid_command_msg =
  {|
  Not a valid command. Possible command:
      roll         --Roll a dice to begin stepping
      inventory    --Check your inventory
      quit         --Warning: terminate the game
  |}

let invalid_name_msg =
  "Your name must be less than 9-character long and non-empty. Try \
   again."

(* printline shortcut *)
let simple_print = ANSITerminal.print_string [ ANSITerminal.white ]
let fancy_print = ANSITerminal.print_string [ ANSITerminal.green ]
let danger_print = ANSITerminal.print_string [ ANSITerminal.red ]

(** [get_num_player_helper] returns the user entered number of players. *)
let rec get_num_player_helper () =
  fancy_print "Welcome to the Cornell simulator! \n";
  print_endline "Enter the number of players (2 to 4 inclusive):";
  print_string "> ";
  try
    match read_line () with
    | exception End_of_file -> raise (failwith "")
    | a_message ->
        let n = int_of_string a_message in
        if n > 1 && n < 5 then n
        else raise (failwith "not valid user num")
  with _ ->
    print_endline "That was not a valid number, please try again.";
    get_num_player_helper ()

let name_count = ref 1

(** [names_helper n acc] returns the user entered names as a player
    list. *)
let rec names_helper n acc =
  try
    if !name_count > n then acc
    else (
      print_endline
        ("Please enter player "
        ^ string_of_int !name_count
        ^ "'s name: ");
      print_string "> ";
      match read_line () with
      | exception End_of_file -> raise InvalidName
      | a_message ->
          let a_name = String.trim a_message in
          if String.length a_name <= 8 && String.length a_name > 0 then (
            name_count := !name_count + 1;
            names_helper n [ init_player a_name ] @ acc)
          else raise InvalidName)
  with InvalidName ->
    print_endline invalid_name_msg;
    names_helper n acc

(**[prompt_dice] returns a random int between 1 and 6, inclusive*)
let rec prompt_dice = 2

(* [continue] handles each iteration of the game, including showing it
   is which player's turn, prompt the player for action, e.g. rolling
   dice) *)

let rec print_path path player (state : State.state) (model : Model.t) =
  if List.length path < 1 then ()
  else
    let loc_move = List.(path |> rev |> hd) in
    let player_moved = { player with location = loc_move } in
    let new_player_list =
      List.mapi
        (fun i x ->
          if i = get_current_player state then player_moved else x)
        (get_players_list state)
    in
    Unix.sleepf 0.5;
    Viewer.update_and_print_board model
      (simple_update state new_player_list);
    print_endline "\n";
    print_path
      (List.filter (fun x -> x <> loc_move) path)
      player state model

(** [winning_msg st] checks if there exist a player that have gpa
    greater than or equal to 4.3, then display winning message and quit.
    Else, print an empty string. *)
let winning_msg st =
  let p_lst = get_players_list st in
  let winner_lst = check_winning st in
  if List.length p_lst == 1 then begin
    print_endline
      ("Congradulations "
      ^ (List.hd p_lst |> get_name)
      ^ "! You have won the game.");
    print_endline winning_ascii;
    Stdlib.exit 0
  end
  else if winner_lst <> [] then begin
    print_endline
      ("Congradulations " ^ List.hd winner_lst
     ^ "! You have won the game.");
    print_endline winning_ascii;
    Stdlib.exit 0
  end
  else print_string ""

(** [continue] handles each iteration of the game, including showing it
    is which player's turn, prompt the player for action, e.g. rolling
    dice) *)
let rec continue (init_st : state) model =
  winning_msg init_st;
  let st = check_mood init_st in
  winning_msg st;
  update_and_print_board model st;
  print_endline
    ("It's "
    ^ get_name (List.nth (get_players_list st) (get_current_player st))
    ^ "'s turn.");
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Please enter a command:";
  print_newline ();
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | a_message -> (
      try
        let a_parsed_command = parse a_message in
        match_player_cmd a_parsed_command st model
      with failure ->
        print_endline invalid_command_msg;
        continue st model)

and match_player_cmd a_parsed_command st model =
  match a_parsed_command with
  | Quit ->
      print_endline "You entered quit. Goodbye!";
      Stdlib.exit 0
  | Roll ->
      Random.self_init ();
      roll_state (Random.int 6 + 1) st model
  | Inventory ->
      List.nth (get_players_list st) (get_current_player st)
      |> get_inventory;
      continue st model
  | _ ->
      print_endline cmd_err_msg;
      continue st model

and roll_state dice st model =
  print_endline ("You rolled a " ^ string_of_int dice);
  let player = List.nth (get_players_list st) (get_current_player st) in
  let path = Model.move dice player.location [] model in
  print_path path player st model;
  let new_st = step path st model in
  continue new_st model

let data_dir_prefix = "data" ^ Filename.dir_sep

let play_game st model =
  let st = init_state (List.rev st) in
  print_endline "Start the game";
  continue st model

(** [read_file_name ] check if the file exists. If not, recursively call
    new prompt to enter file name. *)
let rec read_file_name (st : player list) =
  match read_line () with
  | exception End_of_file ->
      danger_print "\nI didn't see anything. Eh oh.\n";
      read_file_name st
  | file_name -> begin
      let fn = data_dir_prefix ^ file_name ^ ".json" in
      try
        let ic = open_in fn in
        close_in ic;
        let model = fn |> Yojson.Basic.from_file |> from_json in
        play_game st model
      with exn ->
        danger_print
          "\nThis is not a valid file! Maybe you made a typo?\n";
        simple_print "> ";
        read_file_name st
    end

let rec main () =
  let num_player = get_num_player_helper () in
  let st =
    try names_helper num_player [] with InvalidName -> failwith ""
  in
  print_endline
    "Please enter the name of the game file you want to load.";
  print_string "> ";
  read_file_name st

let () = main ()
