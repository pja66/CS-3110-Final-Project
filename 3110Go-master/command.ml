open Board 

(** [player] is a datatype. It represents player.*)
type player ={name: string; score: float} 
(** [players] is a datatype. It represents both players or player and CPU.*)
type players = (player*player)
(**[move] is a datatype. It repsents the 2 types of moves in go: pass or move *)
type move = 
  | Move of coords
  | Pass

(**  [move] is a datatype it repsents the entire game that is currently
     being played.
 * AF: Past is a list pn,...p0 of past game states, where p0 
 * is the initial state and pn is the current state.  Future is a list f0
 * ...fn, where f0 is the state resulting after the original move to pn, 
 * and f1 is the state after f0, continuing until fn is from fn-1 
 * RI: Past is never empty and the last element of past is always the 
     initial game state *)
type game = {past: game_state list; future: game_state list}

and game_state = {board: Board.t; 
                  ps: players; 
                  turn: Board.color; 
                  last_move: move option}

(** [GameEnd] is an exception.*)
exception GameEnd
(** [KohViolated] is an exception. It is used when a Koh is violated.*)
exception KohViolated

let debug = false

(** [rep_ok] takes as parameters: [game].
    @return a [game] if the rep is not violated, else an exception.*)
let rep_ok (g:game) : game = 
  if not debug then g else begin
    match List.rev g.past with 
    | h::t -> if Board.is_empty h.board 
              && h.turn = White 
              && h.last_move = None then g
      else failwith "Invariant violated: oldest state should be initial"
    | [] -> failwith "Invariant violated: should never be empty"
  end

(** [rep_ok] takes as parameters: [game].
    @return a [game] if the rep is not violated, else an exception.*)
let undo (g:game) : game = 
  match g with 
  | {past = init::[]; future = f} -> raise (Board.MoveError "Cannot undo before 
              any moves are made")
  | {past = h::t; future = f} -> {past = t; future = h::f}
  | _ -> failwith "RI violated"

(** [redo] takes as parameters: [game].
    @return a [game] with the past 2 moves erased.*)
let redo (g:game) : game = 
  match g with 
  | {past = p; future = []} -> raise (Board.MoveError "No moves to re-do")
  | {past = p; future = h::t} -> {past = h :: p; future = t}

(** [create_player] takes as parameters: [string] representing a name and a 
    [float] representing a handicap.
    @return a [player].*)
let create_player (n:string) (handicap:float) : player =  
  {name = n; score = handicap}

(** [start_game] takes as parameters: [int] representing the size of the board, 
    and 2 players of type [player]
    @return a new [game].*)
let start_game (i:int) (white:player) (black:player) : game = 
  let gb = Board.init i in 
  let cg = 
    {board = gb; ps = (white,black); turn = White; last_move = None}
  in {past = cg :: []; future = []}

(** [add_score] takes as parameters: [game_state] and a [float] 
    representing the score that will be added.  
    @return a new [player] record with an updated score.*)
let add_score (g:game_state) (s:float) : players = 
  match g.ps with 
  | (w,b) -> 
    if g.turn = White then ({w with score= w.score +. s },b) 
    else (w,{b with score= b.score +. s}) 

(** [get_curr] takes as parameters: [game].  
    @return the current [game_state].*)
let get_curr (g:game) : game_state = 
  List.hd g.past

(** [get_curr] takes as parameters: [game], the current game prior to a move,
    and [input], the board resulting from making the move.  If this board is the
    same as the one after the same player's last move, this violates the 
    Koh rule, and we return true.  Otherwise, return false *)
let koh_detected (g:game) (input:Board.t) = 
  match g with 
  | {past = []; _} -> false;
  | {past = a :: b :: tail; _} -> input = b.board
  | _ -> false

(** [make_move] takes as parameters: [game], and [move] which represents 
    desired move.  
    @return a [game] with the move placed on the board.*)
let make_move (g:game) (mv:move) : game = 
  let old_game = g |> rep_ok |> get_curr in 
  match mv with 
  | Pass -> begin 
      if old_game.last_move = Some Pass then raise GameEnd     
      else let new_game = 
             {old_game with 
              turn = Board.opposite old_game.turn; 
              last_move = Some mv
             } in {past = new_game :: g.past; 
                   future = []}
    end
  | Move c -> begin
      match Board.move old_game.turn c (Board.copy old_game.board) with
      | {board = b; score = s} -> 
        if koh_detected g b then raise (Board.MoveError "Koh Violated") 
        else 
          let new_game = 
            {board = b;
             ps = add_score old_game (float_of_int s);
             turn = Board.opposite old_game.turn;
             last_move = Some mv
            }
          in {past = new_game :: g.past; 
              future = []}
    end

(** [get_next_state] takes as parameters: [game_state], and [move] which represents 
    desired move.  
    @return a [game_state] with the move placed on the board.*)
let get_next_state (cur_state:game_state) (mv:move) : game_state =
  match mv with 
  | Pass -> 
    {cur_state with turn = Board.opposite cur_state.turn; last_move = Some mv}
  | Move c ->
    match Board.move cur_state.turn c (Board.copy cur_state.board) with
    | {board = b; score = s} -> 
      {board = b;
       ps = add_score cur_state (float_of_int s);
       turn = Board.opposite cur_state.turn;
       last_move = Some mv}

(** [end_game] takes as parameters: [game]
    @return a [game] with modified feilds that repsent the game is over.*)
let end_game (g:game) : game = 
  match g with 
  | {past = h::t; future = _} -> begin 
      match h.ps with 
      | (w,b) -> let newps = ({w with score = w.score +. float_of_int 
                                                (end_score White h.board)},
                              {b with score = b.score +. float_of_int 
                                                (end_score Black h.board)})
        in let curr = {h with ps=newps}
        in {g with past = curr :: t}
    end
  | _ -> failwith "RI violated"

let string_of_color = function
  | White -> "White"
  | Black -> "Black"

(** black_winner is var of type [string]. It represents graphic displayed 
    when black wins.*)
let black_winner = "
  ____  _            _     __          ___           _ 
 |  _ \\| |          | |    \\ \\        / (_)         | |
 | |_) | | __ _  ___| | __  \\ \\  /\\  / / _ _ __  ___| |
 |  _ <| |/ _` |/ __| |/ /   \\ \\/  \\/ / | | '_ \\/ __| |
 | |_) | | (_| | (__|   <     \\  /\\  /  | | | | \\__ \\_|
 |____/|_|\\__,_|\\___|_|\\_\\     \\/  \\/   |_|_| |_|___(_)"

(** white_winner is var of type [string]. It represents graphic displayed 
    when white wins.*)
let white_winner = "
 __          ___     _ _        __          ___           _ 
 \\ \\        / / |   (_) |       \\ \\        / (_)         | |
  \\ \\  /\\  / /| |__  _| |_ ___   \\ \\  /\\  / / _ _ __  ___| |
   \\ \\/  \\/ / | '_ \\| | __/ _ \\   \\ \\/  \\/ / | | '_ \\/ __| |
    \\  /\\  /  | | | | | ||  __/    \\  /\\  /  | | | | \\__ \\_|
     \\/  \\/   |_| |_|_|\\__\\___|     \\/  \\/   |_|_| |_|___(_)"

(** [winner] takes as parameters: [game].  
    @return a unit; prints to the terminal the winner.*)
let winner (g:game) = 
  let curr = get_curr g in 
  let w = fst curr.ps in 
  let b = snd curr.ps in 
  if w.score > b.score then begin
    ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.black;] 
      white_winner; print_endline ""; print_endline "";
    print_string (w.name ^ " (White)" ^ " won with a score of ");
    print_float w.score; print_string (" to "); print_float b.score;
    print_endline "\n";
  end else begin
    ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.white;] 
      black_winner; print_endline ""; print_endline "";

    print_string (b.name ^ " (Black)" ^ " won with a score of ");
    print_float b.score; print_string (" to "); print_float w.score;
    print_endline "\n";
  end

(** [turn] takes as parameters: [game].  
    @return a string that repsents what color's move it is.*)
let turn (g:game) = 
  let curr = get_curr g in 
  match curr.turn with 
  | White -> let w = fst (curr.ps) in w.name ^ " (White) to move:" 
  | Black -> let b = snd (curr.ps) in b.name ^ " (Black) to move:"

(** [print_game] takes as parameters: [game].  
    @return a unit; The game is printed to the terminal.*)
let print_game (g:game) = 
  let curr = get_curr g in 
  let w = fst (curr.ps) in 
  let b = snd (curr.ps) in 
  print_endline "Scores:";
  print_endline (w.name ^ " (White): " ^ (string_of_float w.score));
  print_endline (b.name ^ " (Black): " ^ (string_of_float b.score));
  Board.print_board curr.board
