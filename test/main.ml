open OUnit2
open Game
open Model
open State
open Viewer
open Yojson.Basic.Util

(**********************************************************************
  Testing plan.
  ********************************************************************

  1. Automatically tested by OUnit vs. manually tested:

  Since we have created a working terminal based user interface since
  MS2, some testing that involves a lot of interactivity with the users
  are therefore done using play test, including crossing the bridge,
  buying an item, and using an item. We used play test to test these
  functionalities. As we have a interface that display each player's
  gpa, brb, and mood, and command for checking inventory, we can
  effectively see if the user's actions resulted in the right changes.
  Similarly, by counting the number of steps and the result from rolling
  the dice, we can make sure the player end up in the correct location
  after each term.

  For OUnit test, we ensured the parsing functions are correct using
  both black box and glass box testing (below). Then, the play test
  above ensured that if the node information and player information are
  same as displayed on the screen, and thus the program should produce
  the correct information.

  2. What modules were tested by OUnit and how test cases were
  developed:

  The Model, State, and Viewer are tested by OUnit. We tested the
  functions that do not require user input. The Model is heavily tested
  and the State and Viewer are less tested due to the lack of user
  input.

  We have done both black box and glass box. For black box, we tested
  based on the spec of move to ensure that every way of traverse the map
  is possible. For glass box, we cover as many pattern match as
  possible. We develop 2 to 4 test cases for each function.

  3. Why the testing approach demonstrates the correctness of the
  system:

  We also use edge cases, such as a small map with only one node, the
  check winning with strict upper bounds, to test the game. In addition
  to the play test, OUnit test with black box and glass box
  methodologies, we have also played the game with people from outside
  of the project. They are not with the rules of the games, therefore
  likely to come up with unconventional ways to play and potentially
  break the system.

  **)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [string_of_list l] converts l into a string representation of the
    list. *)
let string_of_list lst =
  List.fold_left (fun acc x -> acc ^ "\"" ^ x ^ "\"") "[" lst ^ "]"
  
  (** [tuple_lst_to_string l] converts l into a string representation of the
    list. *)
  let tuple_lst_to_string lst =
  List.fold_left
    (fun acc (a, b) ->
      "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")" ^ acc)
    "" lst

(********************************************************************
   End helper functions.
 ********************************************************************)
let cornell = from_json (Yojson.Basic.from_file "data/cornell.json")
let eng_quad = from_json (Yojson.Basic.from_file "data/eng_quad.json")

let small_json =
  {|
  {
    "areas": [
      {
        "area":"testing area",
        "loop":1,
        "description":"This is a test.",
        "nodes":[
          {
            "id":1,
            "node_info":{
              "ordinary":{
                "description": "this is an ordinary node",
                "is_star":false
              } 
            }
          }
        ]
      }
    ],
    "start node": {
      "loop": 1,
      "id": 1
    },
    "rnd events":[
      {
        "description": "You played Genshin Impact to relax",
        "brb_change": 0,
        "gpa_change": -0.1,
        "mood_change": 2
		  }
    ]
  }
 |}

let small_model = from_json (Yojson.Basic.from_string small_json)

let description_test
    (name : string)
    (input : t)
    (index : int * int)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (tup_to_node index input |> get_description)
    ~printer:String.trim

let move_test
    (name : string)
    i
    starting_pos
    res_list
    model
    expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (move i starting_pos res_list model)
    ~printer:tuple_lst_to_string

let player_1 =
  {
    name = "Tony";
    brb = 100;
    gpa = 2.9;
    mood = 12;
    location = (1, 4);
    gadget = [ "coffee" ];
  }

let player_2 =
  {
    name = "Catherine";
    brb = 0;
    gpa = 3.9;
    mood = 20;
    location = (1, 9);
    gadget = [];
  }

let player_3 =
  {
    name = "Martha";
    brb = 0;
    gpa = 4.3;
    mood = 20;
    location = (1, 9);
    gadget = [];
  }

let player_4 = { player_3 with gpa = 4.2 }

let player_1_influnced_by_event_1 =
  { player_1 with brb = 100; gpa = 2.8; mood = 14 }

let player_1_influnced_by_event_2 =
  { player_1 with brb = 100; gpa = 3.0; mood = 13 }

let player_2_influnced_by_event_3 =
  { player_2 with brb = 0; gpa = 4.0; mood = 19 }

let event_1 = ("You played Genshin Impact to relax", 0, -0.1, 2)
let event_2 = ("You went to study with your friends", 0, 0.1, 1)
let event_3 = ("You studied alone in olin library", 0, 0.1, -1)
let state1 = init_state [ player_1 ]
let state2 = init_state [ player_2 ]
let state_winning = init_state [ player_3 ]
let state_not_win = init_state [ player_4 ]
let node1 = tup_to_node (1, 5) cornell
let node2 = tup_to_node (1, 1) cornell
let node3 = tup_to_node (2, 4) cornell

let check_winning_test name st expected_output =
  name >:: fun _ ->
  assert_equal expected_output (check_winning st)
    ~printer:string_of_list

let shop_gadget_names_test name node expected_output =
  name >:: fun _ ->
  assert (cmp_set_like_lists expected_output (shop_gadget_names node))

let shop_gadget_names_exception_test name node =
  name >:: fun _ ->
  assert_raises (Failure "not a shop node") (fun () ->
      shop_gadget_names node)

let get_gadget_price_test testname name node expected_output =
  testname >:: fun _ ->
  assert_equal expected_output
    (get_gadget_price name node)
    ~printer:string_of_int

let get_gadget_price_exception_test testname name node =
  testname >:: fun _ ->
  assert_raises (Failure "not a shop node") (fun () ->
      get_gadget_price name node)

let get_gadget_mood_influene_test testname name node expected_output =
  testname >:: fun _ ->
  assert_equal expected_output
    (get_gadget_mood_influene name node)
    ~printer:string_of_int

let update_test
    (name : string)
    (path : (int * int) list)
    state
    (model : Model.t)
    expected_output =
  name >:: fun _ ->
  assert_equal expected_output (update_player path state model)

let node_ordinary = tup_to_node (1, 3) cornell
let node_event = tup_to_node (1, 2) cornell
let node_bridge = tup_to_node (2, 5) cornell
let node_shop_1 = tup_to_node (1, 5) cornell
let node_shop_2 = tup_to_node (2, 4) cornell
let node_bus = tup_to_node (1, 1) cornell
let small_node_lst = [ tup_to_node (1, 1) small_model ]

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let tuple_lst_by_area area_num =
  let int_lst = 1 -- 12 in
  List.map (fun i -> (area_num, i)) int_lst

let node_lst_by_area model area_num =
  let coord_lst = tuple_lst_by_area area_num in
  List.map (fun n -> tup_to_node n model) coord_lst

let is_ordinary_test name node expected_output =
  name >:: fun _ ->
  assert_equal expected_output (is_ordinary node)
    ~printer:string_of_bool

let is_shop_test name node expected_output =
  name >:: fun _ ->
  assert_equal expected_output (is_shop node) ~printer:string_of_bool

let is_event_test name node expected_output =
  name >:: fun _ ->
  assert_equal expected_output (is_event node) ~printer:string_of_bool

let is_teleport_test name node expected_output =
  name >:: fun _ ->
  assert_equal expected_output (is_teleport node)
    ~printer:string_of_bool

let get_name_test name player expected_output =
  name >:: fun _ -> assert_equal expected_output (get_name player)

let get_location_test name player expected_output =
  name >:: fun _ -> assert_equal expected_output (get_location player)

let get_brb_test name player expected_output =
  name >:: fun _ -> assert_equal expected_output (get_brb player)

let get_gpa_test name player expected_output =
  name >:: fun _ -> assert_equal expected_output (get_gpa player)

let get_mood_test name player expected_output =
  name >:: fun _ -> assert_equal expected_output (get_mood player)

let rand_node_test
    (name : string)
    (model : Model.t)
    (expected_output : int * int) =
  name >:: fun _ -> assert_equal expected_output (rand_node model)

let rand_event_test
    (name : string)
    (model : Model.t)
    (expected_output : string * int * float * int) =
  name >:: fun _ -> assert_equal expected_output (rand_event model)

let change_name_test
    (name : string)
    (inputed_name : string)
    player
    (expected_output : player) =
  name >:: fun _ ->
  assert_equal expected_output (change_name inputed_name player)

let item_exist_test
    (name : string)
    (inputed_name : string)
    player
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (item_exist player inputed_name)

let get_desctination_test
    (name : string)
    (tup : int * int)
    (model : Model.t)
    (expected_output : int * int) =
  name >:: fun _ ->
  assert_equal expected_output (get_destination tup model)

let event_influence_player_test
    (name : string)
    (player : player)
    (event : string * int * float * int)
    (expected_output : player) =
  name >:: fun _ ->
  assert_equal expected_output (event_influence_player player event)

let get_node_lst_by_index_test
    (name : string)
    (index : int)
    (model : Model.t)
    (expected_output : node list) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output
    (get_node_lst_by_index index model)

let node_loc_to_board_loc_test
    (name : string)
    (node_loc : int * int)
    (expected_output : int * int) =
  name >:: fun _ ->
  assert_equal expected_output (node_loc_to_board_loc node_loc)

let parse_area_name_test
    (name : string)
    (model : Model.t)
    (area_num : int)
    (expected_output : string * string) =
  name >:: fun _ ->
  assert_equal expected_output (parse_area_name model area_num)

let batch_convert_to_node_test
    (name : string)
    (index : int)
    (node_lst : Model.node list)
    (expected_output : (int * int) list) =
  name >:: fun _ ->
  assert_equal expected_output (batch_convert_to_node index node_lst)

let filter_area_by_tag_test
    (name : string)
    (model : Model.t)
    (x : int)
    (s : string)
    (expected_output : node list) =
  name >:: fun _ ->
  assert_equal expected_output (filter_area_by_tag model x s)

let model_tests =
  [
    description_test "ordinary node from cornell.json" cornell (1, 2)
      "this is a random event!";
    description_test "bus node from cornell.json" cornell (1, 1)
      "This is line 10";
    description_test "teleport node from cornell.json" cornell (1, 11)
      "This is bridge";
    description_test "loop 3 ordinary node from cornell.json" cornell
      (3, 1) "this is an ordinary node";
    update_test "the player simple walks 1 step"
      [ (1, 10) ]
      state2 cornell
      [ { player_2 with location = (1, 10) } ];
    move_test "step once then bus teleport" 3 (2, 1) [] cornell
      (List.rev [ (2, 2); (2, 3); (2, 4) ]);
    move_test "ordinary node step 1" 1 (1, 1) [] cornell [ (1, 2) ];
    move_test "ordinary node step 2" 6 (1, 1) [] cornell
      (List.rev [ (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7) ]);
    move_test "bus counterclockwise after teleport" 6 (3, 1) [] cornell
      (List.rev [ (3, 2); (3, 3); (3, 4); (3, 5); (3, 6); (3, 7) ]);
    is_shop_test "is a shop" node_shop_1 true;
    is_shop_test "is not a shop" node_event false;
    is_event_test "is not an event node" node_ordinary false;
    is_event_test "is an event node" node_event true;
    is_teleport_test "is a teleport node" node_bus true;
    is_teleport_test "is not a teleport node" node_event false;
    change_name_test "change player 1's name from Tony to Tony'" "Tony'"
      player_1
      { player_1 with name = "Tony'" };
    rand_node_test "small model random node (1,1)" small_model (1, 1);
    rand_event_test "small model random node (1,1)" small_model
      ("You played Genshin Impact to relax", 0, -0.1, 2);
    get_node_lst_by_index_test "area 1 cornell" 1 cornell
      (node_lst_by_area cornell 1);
    get_node_lst_by_index_test "area 2 cornell" 2 cornell
      (node_lst_by_area cornell 2);
    get_node_lst_by_index_test "area 3 cornell" 3 cornell
      (node_lst_by_area cornell 3);
    get_node_lst_by_index_test "area 3 eng quad" 3 eng_quad
      (node_lst_by_area eng_quad 3);
    get_node_lst_by_index_test "area 1 small" 1 small_model
      small_node_lst;
    shop_gadget_names_test "shop node with player one gadget" node1
      [ "cake" ];
    shop_gadget_names_test "node with two gadgets" node3
      [ "coffee"; "drink" ];
    shop_gadget_names_exception_test "not a shop node" node2;
    get_gadget_price_test "price of cake is 2 when only 1 gadget" "cake"
      node1 2;
    get_gadget_price_test "price of drink is 3 when 2 gadgets" "drink"
      node3 3;
    get_gadget_price_test "price of coffee is 2 when 2 gadgets" "drink"
      node3 3;
    get_gadget_price_exception_test
      "price of any gadget not a shop node raises failure" "not" node2;
    get_gadget_mood_influene_test "cake inprove mood by 3" "cake" node1
      3;
    check_winning_test "martha has gpa 4.3 and won" state_winning
      [ "Martha" ];
    check_winning_test "catherine has not won" state2 [];
    check_winning_test "clone of martha has gpa 4.2 not win"
      state_not_win [];
  ]

let state_tests =
  [
    item_exist_test "player 1 has coffee in his inventory" "coffee"
      player_1 true;
    item_exist_test "player 2 has not have coffee in his inventory"
      "coffee" player_1 true;
    item_exist_test "player 1 has not have 'asdfad' in his inventory"
      "dfadfds" player_1 false;
    get_name_test "player 1 is tony" player_1 "Tony";
    get_name_test "player 2 is catherine" player_2 "Catherine";
    get_location_test "player 1 is at 1,4" player_1 (1, 4);
    get_location_test "player 2 is at 1,9" player_2 (1, 9);
    get_desctination_test "cornell line 10" (1, 1) cornell (4, 4);
    get_desctination_test "cornell line 10" (4, 4) cornell (1, 1);
    get_desctination_test "cornell line 30" (2, 11) cornell (3, 5);
    get_desctination_test "cornell line 30" (3, 5) cornell (2, 11);
    get_brb_test "player 1 has 100 brb" player_1 100;
    get_brb_test "player 2 has 0 brb" player_2 0;
    get_gpa_test "player 1 has gpa 2.9" player_1 2.9;
    get_gpa_test "player 2 has gpa 3.9" player_2 3.9;
    get_mood_test "player 1 has mood 12" player_1 12;
    get_mood_test "player 2 has mood 20" player_2 20;
    event_influence_player_test "event 1 influence player 1" player_1
      event_1 player_1_influnced_by_event_1;
    event_influence_player_test "event 2 influence player 1" player_1
      event_2 player_1_influnced_by_event_2;
    event_influence_player_test "event 2 influence player 2" player_2
      event_3 player_2_influnced_by_event_3;
  ]

let viewer_tests =
  [
    node_loc_to_board_loc_test "cornell board loc (1,1)" (1, 1) (2, 4);
    node_loc_to_board_loc_test "cornell board loc (4,4)" (4, 4) (20, 7);
    node_loc_to_board_loc_test "eng_quad board loc (2,7)" (2, 7) (11, 7);
    node_loc_to_board_loc_test "eng_quad board loc (3,9)" (3, 9) (29, 3);
    parse_area_name_test "cornell area 1" cornell 1
      ("  West  ", " Campus ");
    parse_area_name_test "eng_quad area 4" eng_quad 4
      ("Duffield", "  Hall  ");
    parse_area_name_test "small area 1" small_model 1
      ("testing ", "  area  ");
    batch_convert_to_node_test "batch convert area 1 cornell" 1
      (node_lst_by_area cornell 1)
      (tuple_lst_by_area 1);
    batch_convert_to_node_test "batch convert area 2 cornell" 2
      (node_lst_by_area cornell 2)
      (tuple_lst_by_area 2);
    batch_convert_to_node_test "batch convert area 4 eng_quad" 4
      (node_lst_by_area eng_quad 4)
      (tuple_lst_by_area 4);
    batch_convert_to_node_test "batch convert small model" 1
      small_node_lst
      [ (1, 1) ];
    filter_area_by_tag_test "bus node in area 1 cornell" cornell 1 "bus"
      [ node_bus ];
    filter_area_by_tag_test "shop node in area 2 cornell" cornell 2
      "shop" [ node_shop_2 ];
  ]

let suite =
  "test suite for game"
  >::: List.flatten [ model_tests; state_tests; viewer_tests ]

let _ = run_test_tt_main suite
