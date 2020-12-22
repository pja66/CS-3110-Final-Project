open OUnit2
open Board 
open Command
open Ai


(* A note on our testing strategy:

   In general, we test every interface function, with the majority being 
   via OUnit test suites found in this file.  We also perform some testing 
   through playtesting.  However, all of the functionality of the game itself
   is rigorously tested through OUnit testing in order to ensure correctness. 
   This is much more reliable than playtesting due to the fact that standard 
   go games do not necessarily involve edge cases.  Via stress testing the 
   functions here we ensure that they work during gameplay.board. 

   Specifically, for the interface board.mli, we have dedicated test suites for 
   init, is empty, copy, move, is_valid_move, characterize_neighbors, 
   and end_score.  Opposite was trivial and was tested in utop/via being a helper
   to almost every other function involved.  String_of_board was tested via 
   make play, since it prints to the command line.board

   For command.mli, we have dedicated test suites for create_player, start_game, 
   make_move, end_game, and get_next_state.  Winner outputs a unit and was thus
   tested via make play with several distinct cases.  Turn was also tested this 
   way as it is trivial and its output is printed every turn.  Get_curr was 
   tested via being a helper function for other functions in OUnit test suites.
   Print_game outputs a unit and so it was tested repeatedly via make play. 

   For Ai.mli, we tested via a combination of test suites and playtesting.  
   We have dedicated suites for heuristic, generate_move_list, next_states, 
   max_leaf, and min_leaf, and choose_opt_move as these are the functions which
   are the brains of the AI.  We also playtested the AI to verify its 
   functionality and ensure it ran quickly, which it did.

   For server, we tested via connecting to it and checking that all the 
   functionality is present, as OUnit would not work with it.  The same is true
   for visual, which we playtested as it outputs to the command line.
*)
let vm_printer = function
  | Yes i -> "Yes(" ^ string_of_int i ^ ")"
  | No s -> "No: " ^ s

(* Is_empty *)
let tiny_empty  = init 1
let small_empty = init 5 
let full_empty = init 19

let copy_test_a = Array.make_matrix 5 5 Empty;; 
copy_test_a.(2).(2) <- Piece Black;;
let copy_test_b = Board.copy copy_test_a;;
copy_test_a.(2).(2) <- Piece White;;
let copy_test_ideal = Array.make_matrix 5 5 Empty;;
copy_test_ideal.(2).(2) <- Piece Black;;

let small_single = Array.make_matrix 5 5 Empty;; 
small_single.(2).(2) <- Piece Black;; 

let small_white_circle = Array.make_matrix 5 5 Empty;; 
small_white_circle.(1).(2) <- Piece White;; 
small_white_circle.(3).(2) <- Piece White;; 
small_white_circle.(2).(1) <- Piece White;; 
small_white_circle.(2).(3) <- Piece White;; 

let small_black_circle = Array.make_matrix 5 5 Empty;; 
small_black_circle.(1).(2) <- Piece Black;; 
small_black_circle.(3).(2) <- Piece Black;; 
small_black_circle.(2).(1) <- Piece Black;; 
small_black_circle.(2).(3) <- Piece Black;; 

let concentric = Board.copy small_black_circle;; 
concentric.(0).(2) <- Piece White;; 
concentric.(4).(2) <- Piece White;; 
concentric.(1).(1) <- Piece White;; 
concentric.(1).(3) <- Piece White;; 
concentric.(2).(0) <- Piece White;; 
concentric.(2).(4) <- Piece White;; 
concentric.(3).(1) <- Piece White;; 
concentric.(3).(3) <- Piece White;; 

let deep_recurse = Board.copy small_white_circle;; 
deep_recurse.(0).(2) <- Piece White;; 
deep_recurse.(0).(4) <- Piece White;; 
deep_recurse.(1).(1) <- Piece White;; 
deep_recurse.(1).(3) <- Piece White;; 
deep_recurse.(2).(0) <- Piece White;; 
deep_recurse.(2).(4) <- Piece White;; 
deep_recurse.(3).(1) <- Piece White;; 
deep_recurse.(3).(3) <- Piece White;; 

let path = Array.make_matrix 6 6 Empty;; 
path.(1).(1) <- Piece Black;; 
path.(2).(1) <- Piece Black;; 
path.(3).(1) <- Piece Black;; 
path.(4).(1) <- Piece Black;; 
path.(5).(1) <- Piece Black;; 
path.(1).(2) <- Piece Black;; 
path.(2).(2) <- Piece White;; 
path.(3).(2) <- Piece White;; 
path.(4).(2) <- Piece White;; 
path.(5).(2) <- Piece Black;; 
path.(1).(3) <- Piece Black;; 
path.(3).(3) <- Piece Black;; 
path.(4).(3) <- Piece White;; 
path.(5).(3) <- Piece Black;; 
path.(0).(4) <- Piece White;; 
path.(1).(4) <- Piece White;; 
path.(2).(4) <- Piece White;; 
path.(3).(4) <- Piece White;; 
path.(4).(4) <- Piece White;; 
path.(5).(4) <- Piece Black;; 
path.(1).(5) <- Piece Black;; 
path.(3).(5) <- Piece Black;; 
path.(4).(5) <- Piece Black;; 
path.(5).(5) <- Piece Black;; 
path.(2).(5) <- Piece White;;

let fullboard = Array.make_matrix 5 5 (Piece Black);;
fullboard.(0).(0) <- Empty;;
fullboard.(1).(1) <- Piece White;; 
fullboard.(1).(2) <- Piece White;; 
fullboard.(1).(3) <- Piece White;; 
fullboard.(2).(1) <- Piece White;; 
fullboard.(2).(2) <- Piece White;; 
fullboard.(2).(3) <- Piece White;; 
fullboard.(3).(1) <- Piece White;; 
fullboard.(3).(2) <- Piece White;; 
fullboard.(3).(3) <- Piece White;; 

let four_prisoner_capture_avail = Array.make_matrix 5 5 Empty;;
four_prisoner_capture_avail.(1).(0) <- Piece White;;
four_prisoner_capture_avail.(2).(0) <- Piece White;;
four_prisoner_capture_avail.(0).(1) <- Piece White;;
four_prisoner_capture_avail.(1).(1) <- Piece Black;;
four_prisoner_capture_avail.(2).(1) <- Piece Black;;
four_prisoner_capture_avail.(3).(1) <- Piece White;;
four_prisoner_capture_avail.(1).(2) <- Piece White;;
four_prisoner_capture_avail.(2).(2) <- Piece Black;;
four_prisoner_capture_avail.(3).(2) <- Piece Black;;
four_prisoner_capture_avail.(4).(2) <- Piece White;;
four_prisoner_capture_avail.(2).(3) <- Piece White;;

let corner = {x=0;y=0}
let middle = {x=2;y=2}
let off = {x=0;y=10}
let path_spot   = {x=2;y=3}
let f_p_place = {x=3;y=3}

(* For testing move *)

let small_empty_ideal_w = Array.make_matrix 5 5 Empty;; 
let small_empty_ideal_b = Board.copy small_empty_ideal_w;;
small_empty_ideal_w.(2).(2) <- Piece White;;
small_empty_ideal_b.(2).(2) <- Piece Black;;

let sbc_ideal = Board.copy small_black_circle;;
sbc_ideal.(2).(2) <- Piece Black;;

let concentric_copy = Board.copy concentric;;
let concentric_ideal = Board.copy concentric;;
concentric_ideal.(1).(2) <- Empty;;
concentric_ideal.(3).(2) <- Empty;;
concentric_ideal.(2).(1) <- Empty;;
concentric_ideal.(2).(3) <- Empty;;
concentric_ideal.(2).(2) <- Piece White;;

let dr_copy = Board.copy deep_recurse;;
let dr_ideal = Board.copy deep_recurse;;
dr_ideal.(2).(2) <- Piece White;;

let si_copy = Board.copy path;;
let si_ideal = Board.copy path;;
si_ideal.(2).(3) <- Piece White;;
si_ideal.(3).(3) <- Empty;;

let full_copy = Board.copy fullboard;;
let full_ideal = Board.copy fullboard;;
full_ideal.(0).(0) <- Piece Black;;
full_ideal.(1).(1) <- Empty;; 
full_ideal.(1).(2) <- Empty;; 
full_ideal.(1).(3) <- Empty;; 
full_ideal.(2).(1) <- Empty;; 
full_ideal.(2).(2) <- Empty;; 
full_ideal.(2).(3) <- Empty;; 
full_ideal.(3).(1) <- Empty;; 
full_ideal.(3).(2) <- Empty;; 
full_ideal.(3).(3) <- Empty;; 

let fpca_copy = Board.copy four_prisoner_capture_avail;;
let fpca_ideal = Board.copy four_prisoner_capture_avail;;
fpca_ideal.(1).(1) <- Empty;;
fpca_ideal.(2).(1) <- Empty;;
fpca_ideal.(2).(2) <- Empty;;
fpca_ideal.(3).(2) <- Empty;;
fpca_ideal.(3).(3) <- Piece White;;
(* For testing game end *)

let simple_open = Array.make_matrix 2 2 Empty;;
simple_open.(0).(0) <- Piece White;;
simple_open.(1).(1) <- Piece Black;;

let simple_enclosed_b = Array.make_matrix 3 3 (Piece Black);;
simple_enclosed_b.(1).(1) <- Empty;;

let simple_enclosed_w = Array.make_matrix 3 3 (Piece White);;
simple_enclosed_w.(1).(1) <- Empty;;

let side_enclosed = Array.make_matrix 3 3 Empty;;
side_enclosed.(0).(0) <- Piece White;; 
side_enclosed.(0).(2) <- Piece White;; 
side_enclosed.(1).(1) <- Piece White;; 
side_enclosed.(2).(1) <- Piece Black;;

let alt_enclosed = Array.make_matrix 3 3 Empty;;
alt_enclosed.(0).(1) <- Piece White;; 
alt_enclosed.(2).(1) <- Piece White;; 
alt_enclosed.(1).(0) <- Piece Black;; 
alt_enclosed.(1).(2) <- Piece Black;; 

let deep_recurse_ge = Array.make_matrix 5 5 (Piece Black);;
deep_recurse_ge.(1).(1) <- Empty;;
deep_recurse_ge.(1).(2) <- Empty;;
deep_recurse_ge.(1).(3) <- Empty;;
deep_recurse_ge.(2).(1) <- Empty;;
deep_recurse_ge.(2).(2) <- Empty;;
deep_recurse_ge.(2).(3) <- Empty;;
deep_recurse_ge.(3).(1) <- Empty;;
deep_recurse_ge.(3).(2) <- Empty;;
deep_recurse_ge.(3).(3) <- Empty;;

let self_intersecting = Board.copy deep_recurse_ge;; 
self_intersecting.(0).(1) <- Empty;;
self_intersecting.(1).(0) <- Empty;;
self_intersecting.(2).(2) <- Piece Black;;

let two_circles = Array.make_matrix 7 7 Empty;;
two_circles.(2).(1) <- Piece Black;;
two_circles.(1).(2) <- Piece Black;;
two_circles.(3).(2) <- Piece Black;;
two_circles.(2).(3) <- Piece Black;;
two_circles.(4).(3) <- Piece White;;
two_circles.(3).(4) <- Piece White;;
two_circles.(4).(5) <- Piece White;;
two_circles.(5).(4) <- Piece White;;

(* Here black has 4 territories and white has 9 *)
let corner_with_dot_and_side_territories = Array.make_matrix 7 7 Empty;;
corner_with_dot_and_side_territories.(1).(0) <- Piece Black;;
corner_with_dot_and_side_territories.(1).(1) <- Piece Black;;
corner_with_dot_and_side_territories.(2).(2) <- Piece Black;;
corner_with_dot_and_side_territories.(3).(2) <- Piece Black;;
corner_with_dot_and_side_territories.(4).(1) <- Piece Black;;
corner_with_dot_and_side_territories.(4).(0) <- Piece Black;;
corner_with_dot_and_side_territories.(3).(6) <- Piece White;;
corner_with_dot_and_side_territories.(3).(5) <- Piece White;;
corner_with_dot_and_side_territories.(3).(4) <- Piece White;;
corner_with_dot_and_side_territories.(4).(3) <- Piece White;;
corner_with_dot_and_side_territories.(5).(3) <- Piece White;;
corner_with_dot_and_side_territories.(6).(3) <- Piece White;;



let all_black = Array.make_matrix 7 7 (Piece Black);;

let size = 25
let big = Array.make_matrix size size Empty;;
big.(0).(0) <- Piece White;;
big.(size-1).(size-1) <- Piece Black;;

let string_of_location = function
  | Empty -> " "
  | Piece White -> "W"
  | Piece Black -> "B"

let string_of_board board = 
  let accumulate s l = s ^ string_of_location l in
  let maps = Array.fold_left accumulate in 
  Array.fold_left maps "" board

let string_of_color = function
  | White -> "White"
  | Black -> "Black"

let string_of_move_opt = function
  | None -> "N"
  | Some Pass -> "P"
  | Some Move {x=xi;y=yi} -> "(" ^ string_of_int xi ^ "," ^ string_of_int yi 
                             ^ ")"

let string_of_game_state = function
  | {board = b; ps = players; turn = c; last_move} -> 
    "{[" ^ string_of_board b ^ "], " 
    ^ "(W:" ^ string_of_float (fst players).score
    ^ "; B:" ^ string_of_float (snd players).score ^ "), "  ^
    string_of_color c ^ ", " ^
    string_of_move_opt last_move ^ "}"

let rec string_of_game_aux = function 
  | [] -> ""
  | h::t -> string_of_game_state h ^ string_of_game_aux t

let string_of_game = function 
  | {past = p; future = f} -> "past : [" ^ string_of_game_aux p ^ "] \n" ^ 
                              "future: [" ^ string_of_game_aux f ^ "]"

let string_of_neighbor_data = function 
  | {num_same = ns; num_blank = nb; num_diff = nd} -> 
    "(" ^ (string_of_int ns) ^ "," ^ (string_of_int nb) ^ "," 
    ^(string_of_int nd) ^ ")"

let create_player name score = 
  {name = name; score = score}

let string_of_player = function 
  | {name = n; score = s} -> n ^ (string_of_float s)

let create_game_state board players turn last_move =
  {board = board; ps = players; turn = turn; last_move = last_move}

let rec create_coords_list_aux list acc =
  match list with
  |[] -> acc
  |(x1,y1)::t -> create_coords_list_aux t acc@[{x = x1; y = y1}]

let create_coords_list list =
  create_coords_list_aux list []

let rec string_of_coords_list_aux coords_list acc =
  match coords_list with
  |[] -> acc
  |{x = x1; y = y1}::t -> string_of_coords_list_aux t acc ^ 
                          "(" ^ string_of_int x1 ^ "," ^ string_of_int y1 ^ ")" 

let string_of_coords_list coords_list =
  string_of_coords_list_aux coords_list ""

let is_empty_test
    (name : string) 
    (input_b : Board.t)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_empty input_b)
        ~printer:(string_of_bool)) 

let simple_equality_test
    (name : string)
    (input_b : Board.t)
    (equals : bool)
    (expected_output : Board.t) : test = 
  name >:: (fun _ -> 
      assert_equal (expected_output = input_b) equals
        ~printer:(string_of_bool))


let is_empty_tests = [
  is_empty_test "Simple empty board" small_empty true;
  is_empty_test "Simple non-empty board" small_single false;
  is_empty_test "Larger empty board" full_empty true;
  is_empty_test "Larger non-empty board" big false;
]

let copy_tests = [
  simple_equality_test "B copies data" copy_test_b true copy_test_ideal;
  simple_equality_test "B doesn't copy refs" copy_test_b false copy_test_a;
]

let valid_move_test
    (name : string) 
    (input_t : color) 
    (input_c : coords)
    (input_b : Board.t)
    (expected_output : valid_move) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_valid_move input_t input_c input_b)
        ~printer:(vm_printer)) 

let vm_tests = [
  valid_move_test "Border" White corner tiny_empty 
    (No "A piece cannot be placed here as it has no liberties"); 
  valid_move_test "Empty" Black middle small_empty (Yes 0);
  valid_move_test "SpaceTaken" Black middle small_single
    (No "A piece cannot be placed on top of another");
  valid_move_test "SpaceTaken" White  middle small_single
    (No "A piece cannot be placed on top of another");
  valid_move_test "Offboard" White off small_empty 
    (No "These coordinates are off the board");
  valid_move_test "Suicide" Black middle small_white_circle
    (No "A piece cannot be placed here as it has no liberties");
  valid_move_test "Block"  Black middle small_black_circle (Yes 0);
  valid_move_test "Concentric" White middle concentric (Yes 4);
  valid_move_test "Deeper recursion" White middle deep_recurse (Yes 0);
  valid_move_test  "Self-intersecting" White path_spot path (Yes 1);
  valid_move_test "Full board" Black corner fullboard (Yes 9);
  valid_move_test "AI board" White f_p_place fpca_copy (Yes 4);
]

let move_board_test
    (name : string) 
    (input_t : color) 
    (input_c : coords)
    (input_b : Board.t)
    (expected_output : Board.t) : test = 
  name >:: (fun _ -> 
      let data = (move input_t input_c input_b) in
      assert_equal expected_output data.board
        ~printer:(string_of_board)) 

let move_score_test
    (name : string) 
    (input_t : color) 
    (input_c : coords)
    (input_b : Board.t)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      let data = (move input_t input_c input_b) in
      assert_equal expected_output data.score
        ~printer:(string_of_int)) 

let move_tests = [
  move_board_test "Empty w" White middle small_empty small_empty_ideal_w;
  move_board_test "Empty b" Black middle small_empty small_empty_ideal_b;
  move_score_test "Empty w" White middle small_empty 0;
  move_score_test "Empty b" Black middle small_empty 0;
  move_board_test "Block" Black middle small_black_circle sbc_ideal;
  move_score_test "Block" Black middle small_black_circle 0;
  move_board_test "Concentric" White middle concentric_copy concentric_ideal;
  move_score_test "Concentric" White middle concentric_copy 4;
  move_board_test "Deep recurse" White middle dr_copy dr_ideal;
  move_score_test "Deep recurse" White middle dr_copy 0;
  move_board_test "Self intersecting" White path_spot si_copy si_ideal;
  move_score_test "Self intersecting" White path_spot si_copy 1;
  move_board_test "Full board" Black corner full_copy full_ideal;
  move_score_test "Full board" Black corner full_copy 9;
  move_board_test "AI board" White f_p_place four_prisoner_capture_avail fpca_ideal;
  move_score_test "AI board" White f_p_place four_prisoner_capture_avail 4;
]
let game_end_test
    (name : string)
    (input_t : color)
    (input_b : Board.t)
    (expected_output : int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (end_score input_t input_b)
        ~printer:(string_of_int))

let ge_tests = [
  game_end_test "Simple open b" Black simple_open 0;
  game_end_test "Simple open w" White simple_open 0;
  game_end_test "Simple enclosed b-b" Black simple_enclosed_b 1;
  game_end_test "Simple enclosed b-w" White simple_enclosed_b 0;
  game_end_test "Simple enclosed w-b" Black simple_enclosed_w 0;
  game_end_test "Simple enclosed w-w" White simple_enclosed_w 1;
  game_end_test "Side enclosed" White side_enclosed 1;
  game_end_test "Enclosed by alt colors" White alt_enclosed 0;
  game_end_test "Enclosed by alt colors" Black alt_enclosed 0;
  game_end_test "Enclosed and open (deep recurse)" Black deep_recurse_ge 9;
  game_end_test "Self-interecting path" Black self_intersecting 10;
  game_end_test "Big board b" Black big 0;
  game_end_test "Big board w" White big 0; 
]

let neighbor_test_data = Array.make_matrix 3 3 Empty;;
neighbor_test_data.(0).(0) <- Piece Black;;
neighbor_test_data.(0).(2) <- Piece White;;

let nt_coords = {x=0;y=1}
let nt_ideal = {num_same = 1; num_blank = 1; num_diff = 2}

let cn_test
    (name : string)
    (input_t : color) 
    (input_c : coords)
    (input_b : Board.t)
    (expected_output : neighbor_data) : test = 
  name >:: (fun _ ->
      assert_equal expected_output 
        (characterize_neighbors input_t input_c input_b)
        ~printer:(string_of_neighbor_data))

let cn_tests = [ 
  cn_test "all cases" Black nt_coords neighbor_test_data nt_ideal;
  cn_test "all cases" White nt_coords neighbor_test_data nt_ideal;
]

let player_test
    (name : string)
    (p_name : string)
    (p_hcap : float)
    (expected_output : Command.player) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (Command.create_player p_name p_hcap)
        ~printer:(string_of_player))

let player_tests = [ 
  player_test "Basic" "White" 0. {name="White"; score=0.};
  player_test "Nonzero" "EX" 7. {name = "EX"; score = 7.};
]

let game_eq_test 
    (name : string) 
    (input_g)
    (expected_output : game)  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output input_g
        ~printer:(string_of_game))


let p1 = create_player "test1" 1. 
let p2 = create_player "test2" 2.
let e_o_base = {board = Array.make_matrix 10 10 Empty; 
                ps = (p1,p2);
                last_move = None;
                turn = White} :: []
let e_o = {past = e_o_base; future = []}

let init_tests = [
  game_eq_test "basic" (Command.start_game 10 p1 p2) e_o;
]

let base_board = Array.make_matrix 10 10 Empty;;
let board_1 = Board.copy base_board;;
board_1.(0).(2) <- Piece White;;
let board_2 = Board.copy board_1;; 
board_2.(0).(1) <- Piece Black;;
let board_3 = Board.copy board_2;; 
board_3.(2).(2) <- Piece White;;
let board_4 = Board.copy board_3;; 
board_4.(2).(1) <- Piece Black;;
let board_5 = Board.copy board_4;; 
board_5.(1).(1) <- Piece White;;
let board_6 = Board.copy board_5;; 
board_6.(1).(0) <- Piece Black;;
let board_7 = Board.copy board_6;; 
board_7.(1).(3) <- Piece White;;
let board_8 = Board.copy board_7;;
board_8.(1).(2) <- Piece Black;;
board_8.(1).(1) <- Empty;;

let g_s_0 = List.hd e_o_base
let g_s_1 = {g_s_0 with board = board_1; 
                        turn = Black; 
                        last_move = Some (Move {x=0;y=2})}
let g_s_2 = {g_s_0 with board = board_2; 
                        last_move = Some (Move {x=0;y=1})}
let g_s_3 = {g_s_1 with board = board_3; 
                        last_move = Some (Move {x=2;y=2})}
let g_s_4 = {g_s_0 with board = board_4; 
                        last_move = Some (Move {x=2;y=1})}
let g_s_5 = {g_s_1 with board = board_5; 
                        last_move = Some (Move {x=1;y=1})}
let g_s_6 = {g_s_0 with board = board_6; 
                        last_move = Some (Move {x=1;y=0})}
let g_s_7 = {g_s_1 with board = board_7; 
                        last_move = Some (Move {x=1;y=3})}
let new_b = create_player "test2" 3.
let new_ps = (p1,new_b)
let g_s_8= {g_s_0 with board = board_8; 
                       last_move = Some (Move {x=1;y=2});
                       ps = new_ps}

let past_0 = e_o_base
let past_1 = g_s_1 :: past_0
let past_2 = g_s_2 :: past_1
let past_3 = g_s_3 :: past_2
let past_4 = g_s_4 :: past_3
let past_5 = g_s_5 :: past_4
let past_6 = g_s_6 :: past_5
let past_7 = g_s_7 :: past_6
let past_8 = g_s_8 :: past_7

let e_o_1 = {e_o with past = past_1}
let e_o_2 = {e_o with past = past_2}
let e_o_3 = {e_o with past = past_3}
let e_o_4 = {e_o with past = past_4}
let e_o_5 = {e_o with past = past_5}
let e_o_6 = {e_o with past = past_6}
let e_o_7 = {e_o with past = past_7}
let e_o_8 = {e_o with past = past_8}

let input_0 = e_o
let input_1 = Command.make_move e_o (Move {x=0;y=2})
let input_2 = Command.make_move input_1 (Move {x=0;y=1})
let input_3 = Command.make_move input_2 (Move {x=2;y=2})
let input_4 = Command.make_move input_3 (Move {x=2;y=1})
let input_5 = Command.make_move input_4 (Move {x=1;y=1})
let input_6 = Command.make_move input_5 (Move {x=1;y=0})
let input_7 = Command.make_move input_6 (Move {x=1;y=3})
let input_8 = Command.make_move input_7 (Move {x=1;y=2});;

try let (input_9) = Command.make_move input_8 (Move {x=1;y=1}) in 
  failwith "koh test not passing"
with | Board.MoveError s -> s

let make_move_tests = [ 
  game_eq_test "First move" input_1 e_o_1;
  game_eq_test "Move 2" input_2 e_o_2;
  game_eq_test "Move 3" input_3 e_o_3;
  game_eq_test "Move 4" input_4 e_o_4;
  game_eq_test "Move 5" input_5 e_o_5;
  game_eq_test "Move 6" input_6 e_o_6;
  game_eq_test "Move 7" input_7 e_o_7;
  game_eq_test "Move 8" input_8 e_o_8;
]

let end_1 = Command.end_game e_o_4
let end_2 = Command.end_game e_o_8
let e_end_1 = e_o_4
let new_b1 = create_player "test2" 5.
let new_ps1 = (p1,new_b1)
let e_end_2_state = {g_s_8 with ps = new_ps1}
let alt_end = e_end_2_state :: past_7
let e_end_2 = {e_o with past = alt_end}

let end_game_tests = [
  game_eq_test "End 1" end_1 e_end_1;
  game_eq_test "End 2" end_2 e_end_2;
]

let heuristic_test
    (name : string)
    (ai_color: color)
    (ai_players_prisoners : float)
    (humans_prisoners: float)
    (board : Board.t)
    (prisoners_weight: float)
    (territories_weight: float)
    (eye_weight: float)
    (connected_weight: float)
    (original_move: move)
    (expected_output : float) : test =
  name >:: (fun _ -> assert_equal expected_output 
               (let ai_player = create_player "AI" ai_players_prisoners in
                let players = if ai_color = Black then
                    (create_player "Human" humans_prisoners, ai_player) 
                  else 
                    (ai_player, create_player "Human" humans_prisoners) in
                let state = create_game_state board players Black None in
                heuristic state original_move prisoners_weight 
                  territories_weight eye_weight connected_weight)
               ~printer:(string_of_float))

let heuristic_tests = [
  heuristic_test "Black AI: 5 p 21 t; human: 10 p 0 t" 
    Black 5. 10. small_black_circle 2. 1. 100. 1. (Move {x  = 1; y = 1;}) 11.;
  heuristic_test "Black AI: 0 p 0 t; human: 0 p 0 t" 
    Black 0. 0. alt_enclosed 2. 1. 100. 1.(Move {x  = 1; y = 1;}) 0.;
  heuristic_test "Black AI: 2 p 1 t; human: 4 p 1 t" 
    Black 2. 4. two_circles 1. 1. (- 2.) 1.(Move {x  = 1; y = 1;}) (-2.);
  heuristic_test "White AI: 3 p 9 t; human: 6 p 4 t" 
    White 3. 6. corner_with_dot_and_side_territories 2. 7. 29. 1.
    (Move {x  = 1; y = 1;}) 29.;
]

let generate_move_list_test
    (name : string)
    (board : Board.t)
    (turn: color)
    (ai_color: color)
    (human_color: color)
    (expected_output: (int*int) list) : test =
  name >:: (fun _ -> assert_equal (create_coords_list expected_output) 
               (let ai_player = create_player "AI" 0. in
                let human_player = create_player "Human" 0. in
                let players = if ai_color = Black then 
                    (human_player, ai_player) else 
                    (ai_player, human_player) in
                let state = create_game_state board players ai_color None in
                generate_move_list state) ~printer:string_of_coords_list)

let generate_move_list_tests = [
  generate_move_list_test "poss moves 5x5 two circles board Black AI" 
    two_circles Black Black White 
    [(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(1,0);(1,1);(1,3);
     (1,4);(1,5);(1,6);(2,0);(2,2);(2,4);(2,5);(2,6);(3,0);(3,1);(3,3);(3,5);
     (3,6);(4,0);(4,1);(4,2);(4,6);(5,0);(5,1);(5,2);(5,3);(5,5);(5,6);(6,0);
     (6,1);(6,2);(6,3);(6,4);(6,5);(6,6)];
  generate_move_list_test "poss moves 3x3 alt_enclosed board White human" 
    alt_enclosed White Black White 
    [(0,0);(0,2);(1,1);(2,0);(2,2)];
  generate_move_list_test "poss moves completely filled board: none" 
    all_black Black Black White [];
]

let string_of_float_pair p =
  let string_of_elem1 = p |> fst |> string_of_float in
  let string_of_elem2 = p |> snd |> string_of_float in
  "(" ^ string_of_elem1 ^ "," ^ string_of_elem2 ^ ")"


let get_next_state_test
    (name : string)
    (board : Board.t)
    (ai_prisoners : float)
    (human_prisoners : float)
    (turn: color)
    (ai_color: color)
    (last_move : move option)
    (next_move : move)
    (expected_output: float*float) : test =
  name >:: (fun _ -> assert_equal expected_output
               (let ai_player = create_player "AI" ai_prisoners in
                let human_player = create_player "human" human_prisoners in
                let players = if ai_color = Black then 
                    (human_player, ai_player) else 
                    (ai_player, human_player) in
                let initial_state = create_game_state board players turn 
                    last_move in
                let new_state = get_next_state initial_state next_move in
                let whitescore = (fst new_state.ps).score in
                let blackscore = (snd new_state.ps).score in
                (whitescore,blackscore)) ~printer:string_of_float_pair)

let size_three_with_white_capture = Array.make_matrix 3 3 Empty;;
size_three_with_white_capture.(1).(0) <- Piece White;;
size_three_with_white_capture.(0).(1) <- Piece White;;
size_three_with_white_capture.(2).(1) <- Piece White;;
size_three_with_white_capture.(1).(1) <- Piece Black;;

let get_next_state_tests = [
  get_next_state_test "white human makes a large capture, see if scores update" 
    four_prisoner_capture_avail 7. 1. White Black None (Move {x = 3; y = 3})
    (5.,7.);
  get_next_state_test "white human captures one black prisoner" 
    size_three_with_white_capture 0. 0. White Black None (Move {x = 1; y = 2})
    (1.,0.);
] 

let string_of_move mv =
  match mv with
  |Pass -> "pass"
  |Move {x = x1; y = y1} -> "(" ^ string_of_int x1 ^ "," ^ string_of_int y1 ^")" 

let rec string_of_state_list_aux (state_list: game_state list) (acc: string) = 
  match state_list with
  |[] -> acc
  |h::t -> string_of_state_list_aux t (string_of_game_state h) ^ "\n" ^ acc

let string_of_state_list (state_list:game_state list) = 
  string_of_state_list_aux state_list ""

let next_states_test
    (name : string)
    (board : Board.t)
    (ai_prisoners : float)
    (human_prisoners : float)
    (turn: color)
    (ai_color: color)
    (last_move : move option)
    (expected_output: game_state list) : test =
  name >:: (fun _ -> assert_equal expected_output
               (let ai_player = create_player "AI" ai_prisoners in
                let human_player = create_player "human" human_prisoners in
                let players = if ai_color = Black then 
                    (human_player, ai_player) else 
                    (ai_player, human_player) in
                let initial_state = create_game_state board players turn 
                    last_move in
                next_states initial_state) ~printer:string_of_state_list)

let p_1_test = create_player "human" 2.
let p_2_test = create_player "AI" 4.
let p_1_test_alt = create_player "human" 3.
let ai_test_ps = (p_1_test,p_2_test)
let ai_test_ps_alt = (p_1_test_alt,p_2_test)

let board_data1 = Board.move White {x = 2; y = 2} size_three_with_white_capture
let board_data2 = Board.move White {x = 2; y = 0} size_three_with_white_capture
let board_data3 = Board.move White {x = 1; y = 2} size_three_with_white_capture
let board_data4 = Board.move White {x = 0; y = 0} size_three_with_white_capture
let board_data5 = Board.move White {x = 0; y = 2} size_three_with_white_capture

let g1 = create_game_state board_data1.board ai_test_ps
    Black (Some (Move {x = 2; y = 2}))
let g2 = create_game_state board_data2.board ai_test_ps
    Black (Some (Move {x = 2; y = 0}))
let g3 = create_game_state board_data3.board ai_test_ps_alt
    Black (Some (Move {x = 1; y = 2}))
let g4 = create_game_state board_data4.board ai_test_ps
    Black (Some (Move {x = 0; y = 0}))
let g5 = create_game_state board_data5.board ai_test_ps
    Black (Some (Move {x = 0; y = 2}))

let expected_games_list = g4 :: g5 :: g3 :: g2 :: g1 :: []
let next_states_tests = [
  next_states_test "next states on 3x3 board with potential 
  mcapture for white human"  
    size_three_with_white_capture 4. 2. White Black None
    expected_games_list;

]

let string_of_leaf (l:leaf) : string=
  match l with
  |{state = ss; 
    heuristic_val = value; 
    previous_moves = pm} -> Float.to_string value

let min_leaf_test
    (name : string)
    (leaf1 : leaf)
    (leaf2 : leaf)
    (expected_output: leaf) : test =
  name >:: (fun _ -> assert_equal expected_output (min_leaf leaf1 leaf2) 
               ~printer:string_of_leaf)

let gs1 = create_game_state full_ideal (p1,p2) 
    White (Some (Move {x  = 1; y = 1;}) )
let mve1 = Move {x  = 1; y = 1;}
let mve2 = Move {x  = 0; y = 0;}

let lf1 = {state=  gs1; heuristic_val= 1.0; previous_moves = [mve1]}
let lf2 = {state=  gs1; heuristic_val= 2.0; previous_moves = [mve2]}
let lf3 = {state=  gs1; heuristic_val= 3.0; previous_moves = [mve1]}
let lf4 = {state=  gs1; heuristic_val= 2.0; previous_moves = [mve2]}
let lf5 = {state=  gs1; heuristic_val= 3.0; previous_moves = [mve1]}
let lf6 = {state=  gs1; heuristic_val= -2.0; previous_moves = [mve2]}

let min_leaf_test = [
  min_leaf_test "min basic 1 leaf" lf1 lf2 lf1;
  min_leaf_test "min basic 2 leaf" lf3 lf4 lf4;
  min_leaf_test "min basic 3 leaf" lf6 lf5 lf6;
]


let max_leaf_test
    (name : string)
    (leaf1 : leaf)
    (leaf2 : leaf)
    (expected_output: leaf) : test =
  name >:: (fun _ -> assert_equal expected_output (max_leaf leaf1 leaf2) 
               ~printer:string_of_leaf)

let max_leaf_test = [
  max_leaf_test "max basic 1 leaf" lf1 lf2 lf2;
  max_leaf_test "max basic 2 leaf" lf3 lf4 lf3;
  max_leaf_test "max basic 3 leaf" lf6 lf5 lf5;
]

let choose_opt_move_test 
    (name : string)
    (board : Board.t)
    (tree_depth : int)
    (prisoners_weight: float)
    (territories_weight: float)
    (eye_weight: float)
    (connected_weight: float)
    (expected_output: move) : test =
  name >:: (fun _ -> assert_equal expected_output 
               (let state =  create_game_state board 
                    (create_player "AI" 0., create_player "Human" 0.) 
                    White None in choose_opt_move state tree_depth 
                  prisoners_weight territories_weight
                  eye_weight connected_weight) ~printer:(string_of_move))

let choose_opt_move_tests = [
  choose_opt_move_test "four prisoner capture available for white AI" 
    four_prisoner_capture_avail 2 1. 0. 100. 1. (Move {x = 3; y = 3});
]

let suite = "suite" >::: List.flatten [
    copy_tests;
    is_empty_tests;
    copy_tests;
    vm_tests;
    move_tests;
    ge_tests;
    cn_tests;
    player_tests;
    init_tests;
    make_move_tests;
    end_game_tests;
    heuristic_tests;
    generate_move_list_tests;
    get_next_state_tests;
    next_states_tests;
    choose_opt_move_tests;
    min_leaf_test;
    max_leaf_test;

  ]

let _ = run_test_tt_main suite

