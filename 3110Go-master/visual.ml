open Board 
open Printf
open Command
open Ai  

(** [logo] is var of type [string]. It represents our game logo.*)
let logo = "  
 $$$$$$\\   $$$$$$\\   $$$$$$\\                          $$\\ 
$$  __$$\\ $$  __$$\\ $$  __$$\\                         $$ |
$$ /  \\__|$$ /  $$ |$$ /  \\__| $$$$$$\   $$$$$$\\$$$$\\  $$ |
$$ |$$$$\\ $$ |  $$ |$$ |       \\____$$\\ $$  _$$  _$$\\ $$ |
$$ |\\_$$ |$$ |  $$ |$$ |       $$$$$$$ |$$ / $$ / $$ |$$ |
$$ |  $$ |$$ |  $$ |$$ |  $$\\ $$  __$$ |$$ | $$ | $$ |$$ |
\\$$$$$$  | $$$$$$  |\\$$$$$$  |\\$$$$$$$ |$$ | $$ | $$ |$$ |
 \\______/  \\______/  \\______/  \\_______|\\__| \\__| \\__|\\__|"


(** [gameType] is a datatype. It represents the 2 game 
    opponets player and AI.*)
type gameType = AI |TwoPlayer


type inputs = Left | Right | Up | Down | Pass | Quit | Enter

(**SOURCE: The function below, get1char is from the following source: 
   https://stackoverflow.com/a/13410456
*)

let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

(**  [player_creator] takes as paramters: [gametype]. It creates two players,
     and taking in names and handicaps from CLI for each player. 
     @return unit.*)
let rec player_creator gametype = 
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ANSITerminal.blue] "What is White's name? \n >";

  let name = read_line () in 
  ANSITerminal.print_string [ANSITerminal.blue] "What is White's handicap?\n >";
  let hc = read_int () in let p1 = (create_player name (float_of_int hc)) in 
  if gametype = AI then 
    let p2 = (Command.create_player "AI" 6.5) in (p1,p2)

  else 
    let () = ANSITerminal.print_string [ANSITerminal.blue] 
        "What is Black's name? \n >"; in
    let name2 = read_line () in 
    ANSITerminal.print_string [ANSITerminal.blue] 
      "What is Black's handicap? \n >";
    let hc2 = read_int () in let p2 = (create_player name2 
                                         ((float_of_int hc2) +. 6.5))
    in (p1,p2)

(** [board_size] takes as paramters: [unit]. It prompts the user to 
     choose a board size N, that is between 4 - 19.  
     @return unit.*)
let rec board_size () = 
  ANSITerminal.print_string [ANSITerminal.blue] 
    "What size board? Pick any number between 4 and 19: \n > ";
  let m = read_int () in 
  if (m < 3 || m > 20) then let () =  print_endline 
                                ("You entered an invalid choice. 
                                Try something else \n >"); 
    in board_size () 
  else 
    m

(**  [main_menu] takes as paramters: [unit]. It gets called first to initialize 
     the game type, either verus another human or vs ai.
     Exception raised by search functions when the desired object could 
     not be found.
     @return unit.*)
let rec main_menu () = 
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.blue;] logo;
  ANSITerminal.print_string [ANSITerminal.blue] 
    "\n\nWelcome to GOCaml, the game of Go implemented in OCaml!";
  ANSITerminal.print_string [ANSITerminal.blue] 
    "\nType \"AI\" or \"two player\":\n > ";
  match read_line() with 
  |"Ai"
  |"ai"
  |"AI" -> AI
  |"two player"
  |"2p"
  |"Two player"
  |"Two Player"
  |"multiplayer"
  |"Multiplayer" -> TwoPlayer 
  |_ -> print_endline ("You entered an invalid choice. Try something else"); 
    main_menu ()

(* Constants needed to init AI*)
let tree_depth = 2
let prisoners_weight = 10.
let territories_weight = 0.
let eye_weight = 100.
let connected_weight = 1. 

(*Function that queries the AI. Passes the current game as g *)
let ai_mover (g: Command.game)  =
  let curr = get_curr g in 
  let cords = 
    if List.length g.past < 3 then 
      Ai.first_move curr
    else 
      Ai.choose_opt_move curr tree_depth prisoners_weight territories_weight 
        eye_weight connected_weight 
  in 
  cords

(*get_move is the function that continously gets the the moves until
  the game is over. This is for multiplayer.*)
let rec get_move (g:Command.game) ()  : unit =
  (* if (get_curr g).turn = Black then 
    get_move (make_move g (ai_mover g)) ()
  else *)
    Command.print_game g;
  print_endline (Command.turn g);
  print_endline "Type x coordinate and enter: \n";
  print_endline "Type p to pass or q to quit \n >";
  let x = read_line () in 
  if x = "p" then begin 
    try 
      ANSITerminal.erase Screen; 
      get_move (make_move g Pass) ()
    with 
    |Command.GameEnd -> g |> end_game |> winner; exit 0 end 
  else if x = "q" then 
    exit 0 
  else print_endline "Type y coordinate and enter: \n >";
  let xi = int_of_string_opt x in 
  if xi = None then begin
    ANSITerminal.erase Screen; 
    print_endline "Illegal coordinate"; get_move (g) () end 
  else
    let y = read_int_opt () in 
    match (xi, y) with 
    | (Some xi, Some yi) -> begin 
        try ANSITerminal.erase Screen; 
          get_move (make_move g (Move {x=yi - 1;y=xi - 1})) ()
        with | Board.MoveError e -> 
          begin print_endline ("Illegal move: " ^ e); 
            get_move g () end
             | Command.KohViolated -> 
               begin print_endline "Illegal move: violates the koh rule"; 
                 get_move g () end
      end
    | _ -> ANSITerminal.erase Screen;
      print_endline "Illegal coordinate";
      get_move (g) ()

(*get_move is the function that continously gets the the moves until
  the game is over. This is for the AI.  *)
let rec get_move_ai (g:Command.game) ()  : unit =
  if (get_curr g).turn = Black then 
    get_move_ai (make_move g (ai_mover g)) ()
  else
  Command.print_game g;
  print_endline (Command.turn g);
  print_endline "Type x coordinate and enter: \n";
  print_endline "Type p to pass or q to quit \n >";
  let x = read_line () in 
  if x = "p" then begin 
    try 
      ANSITerminal.erase Screen; 
      get_move_ai (make_move g Pass) ()
    with 
    |Command.GameEnd -> g |> end_game |> winner; exit 0 end 
  else if x = "q" then 
    exit 0 
  else print_endline "Type y coordinate and enter: \n >";
  let xi = int_of_string_opt x in 
  if xi = None then begin
    ANSITerminal.erase Screen; 
    print_endline "Illegal coordinate"; get_move_ai (g) () end 
  else
    let y = read_int_opt () in 
    match (xi, y) with 
    | (Some xi, Some yi) -> begin 
        try ANSITerminal.erase Screen; 
          get_move_ai (make_move g (Move {x=yi - 1;y=xi - 1})) ()
        with | Board.MoveError e -> 
          begin print_endline ("Illegal move: " ^ e); 
            get_move_ai g () end
             | Command.KohViolated -> 
               begin print_endline "Illegal move: violates the koh rule"; 
                 get_move_ai g () end
      end
    | _ -> ANSITerminal.erase Screen;
      print_endline "Illegal coordinate";
      get_move_ai (g) ()


(** main needs to return type unit. gameplay, or whatever the last function is, 
    needs to. Order is crucial here to init game and the *)
(**  [main] takes as paramters: unit. It calls all the above functions to run
     the game.
     @return unit.
*)
let main () = 
  ANSITerminal.resize 100 48;
  let game_type = main_menu () in
  let players = player_creator game_type in 
  let size = board_size () in 
  let game_started = start_game size (fst players) (snd players) in 
  if game_type = TwoPlayer then 
    get_move game_started ()
  else 
    get_move_ai game_started ()

let () = main () 
