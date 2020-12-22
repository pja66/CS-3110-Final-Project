(** Implements board.mli interface *)

(** [valid_move] is a datatype. It represents whether a propsed move is valid.*)
type valid_move = 
  |Yes of int 
  |No of string 

(** [color] is a datatype. It represents the two colors a player can use.*)
type color = White | Black

(** [location] is a datatype. It represents a board square.*)
type location = 
  | Empty
  | Piece of color 

(** [location_extended] is a datatype. It represents the board squares and is  
    implmented to help optimize our DFS.*)
type location_extended = 
  | L of location 
  | Block 
  | Confirmed of bool 

(** [t] is a type: [location array array]. It represents a go board.*)
type t = location array array 
(** [te] is a type: [location_extended array array]. It represents a go board.*)
type te = location_extended array array 
(** [coords] is a type: [{x : int; y : int}].*)
type coords = {x : int; y : int}

(** [game_data] is a datatype. 
    It holds the current board and score of the game.*)
type game_data = {board: t; score: int;}  

(** [neighbor_data is a datatype.] 
    The first field is the number of neighbors of the same color
    The second is the number of blank space neighbors 
    The third is the number of opposite neighbors or borders *)
type neighbor_data = {num_same : int; num_blank : int; num_diff : int}

(** [MoveError] is an exception. It takes as paramters: [string].*)
exception MoveError of string 
(** [PropogateTrue] is an exception. It is used to optimize our DFS search.*)
exception PropogateTrue
(** [PropogateTrue] is an exception. It is used to optimize our DFS search.*)
exception PropogateFalse

(** [init] creates an array of size t. 
    @return a board of type [t].*)
let init (s:int): t  = 
  Array.make_matrix s s Empty

(** [is_empty] takes as parameters: [t].
    @return a [bool] that represents if [t] has no peices.*)
let is_empty (board:t) : bool = 
  let p loc = (loc = Empty) in 
  let mapp = Array.for_all p in 
  Array.for_all mapp board

(** [copy] takes as parameters: a board of type [t].
    @return an identical copy*)
let copy board = 
  let id = (fun x -> x) in 
  let mapf = Array.map id in 
  Array.map mapf board  

(** [convert] takes as parameters: a board of type [t].
    @return an identical copy expect the spaces are repsented with type te 
    as opposed to t.*)
let convert (board:t) : te = 
  let conv = (fun x -> L x) in 
  let convf = Array.map conv in 
  Array.map convf board

(** [opposite] takes as parameters: [color].
    @return the other color. 
    Ex. [opposite] white returns black.*)
let opposite (c:color) = 
  match c with 
  | White -> Black
  | Black -> White

(** [bounds] takes as parameters: [coords] and an [array]. 
    @return a [bool] 
    that represents whether the move is within the bounds of the board.*)
let bounds (c:coords) board : bool = 
  let len = Array.length board in 
  c.x < len && c.x >= 0 && 
  c.y < len && c.y >= 0

(**[open_space] takes as parameters: [coords] and a board of type [t]. 
   @return a [bool] representing that if the desired space is empty.*)
let open_space (c:coords) (board:t) : bool = 
  board.(c.x).(c.y) = Empty

(** [copy_change] takes as parameters: [coords] and a board of type [te]. 
    @return a copy with one "cell" altered.*)
let copy_change (board:te) (c:coords) : te = 
  let nb = copy board in 
  nb.(c.x).(c.y) <- Block; nb

let rec hl_helper turn c board : bool = 
  if not (bounds c board) then false else 
    match board.(c.x).(c.y) with 
    | Block -> false
    | L Piece cp -> if turn = cp then 
        has_liberties turn c (copy_change board c) else false
    | Confirmed true -> raise PropogateTrue
    | Confirmed false -> raise PropogateFalse
    | L Empty -> raise PropogateTrue

(** [has_liberties] takes as parameters: [color], [coords], a board of type [te]
    @return a [bool] that represents whether the desired space has liberities.*)
and has_liberties (turn:color) (c:coords) (board:te) : bool = 
  let right = hl_helper turn {c with x = c.x+1} board in
  let left = hl_helper turn {c with x = c.x-1} board in
  let up = hl_helper turn {c with y = c.y+1} board in
  let down = hl_helper turn {c with y = c.y-1} board in
  right || left || up || down

(** [sum_capture] takes as parameters: a board of type [te], a board of type [t] 
    [color], and an [int] that represents the length of the board. 
    @return an [int] which represents how many peices where captured.*)
let sum_capture (board_ext:te) (board:t) (turn:color) (len:int): int =  
  let count = ref 0 in  
  for xt = 0 to len do 
    for yt = 0 to len do
      match board_ext.(xt).(yt) with 
      | Block -> begin
          board.(xt).(yt) <- Empty;
          count := (!count) + 1;
        end
      | Confirmed b -> begin
          board.(xt).(yt) <- if b then Piece (opposite turn) else Empty;
          count := (!count) + if b then 0 else 1;
        end
      | L l -> board.(xt).(yt) <- l;
    done;
  done;
  !count

(** [capture] takes as parameters: a board of type [te], a board of type [t], 
    [color], and an [int] that represents the length of the board.
    This function can have side effects. 
    @return an [int] which represents how many pieces where captured.*)
let capture (turn:color) (c:coords) (board:t) : int = 
  board.(c.x).(c.y) <- Piece turn;
  let board_extended = convert board in 
  let len = (Array.length board_extended) - 1 in 
  for x = 0 to len do
    for y = 0 to len do
      let ci = {x=x;y=y} in
      if board_extended.(x).(y) = L (Piece (opposite turn)) then begin 
        let b = try has_liberties (opposite turn) ci board_extended with 
          | PropogateTrue -> true
          | PropogateFalse -> false
        in board_extended.(ci.x).(ci.y) <- Confirmed b;
      end
    done;
  done;
  board_extended.(c.x).(c.y) <- L Empty;
  sum_capture board_extended board turn len

(** [is_valid_move] takes as parameters: [color], [coords], 
    and a board of type [t]. This function can have side effects.
    @return [valid_move] repsenting whether the move was valid.*)
let is_valid_move (turn:color) (c:coords) (board:t) : valid_move =
  if bounds c board then begin 
    if open_space c board then begin 
      let count = capture turn c board in 
      if try has_liberties turn c (convert board)
        with | PropogateTrue -> true 
      then Yes count
      else No "A piece cannot be placed here as it has no liberties"
    end else No "A piece cannot be placed on top of another"
  end else No "These coordinates are off the board"

(** [move] takes as paramters: [color], [coords], and a board of type [t].
    This function can have side effects.
    @return [valid_move]  repsenting whether the move was valid.*)
let move (turn:color) (c:coords) (board:t) : game_data = 
  let board_copy = copy board in 
  match is_valid_move turn c board_copy with 
  | Yes score -> board_copy.(c.x).(c.y) <- Piece turn; 
    {board = board_copy; score = score} 
  | No exn -> raise (MoveError exn) 

let rec ic_helper turn c board : bool = 
  if not (bounds c board) then true else
    match board.(c.x).(c.y) with 
    | Block -> true
    | L Piece cp -> if cp = turn then true else raise PropogateFalse
    | L Empty -> is_countable turn c (copy_change board c)
    | Confirmed true -> raise PropogateTrue
    | Confirmed false -> raise PropogateFalse

and is_countable (turn:color) (c:coords) (board:te) : bool =
  (* DEBUG 
     let () = Printf.printf "%d%d\n%!" c.x c.y
     in
     DEBUG *)
  let left = ic_helper turn {c with x = c.x-1} board in
  let up = ic_helper turn {c with y = c.y+1} board in
  let right = ic_helper turn {c with x = c.x+1} board in
  let down = ic_helper turn {c with y = c.y-1} board in
  right && left && up && down

(** [count] takes as paramters: [color], and a board of type [t].
    @return [int] repsenting how many pieces are placed.*)
let count (turn:color) (board:t) : int = 
  let itr sum l= if l = Piece turn then sum+1 else sum in 
  let itr_row i arr= Array.fold_left itr i arr in 
  Array.fold_left itr_row 0 board

(** [sum_end_score] takes as paramters: [color], a board of type [t], 
    a board of type [te], [int] representing the size of the board.
    @return [int] repsenting the final score for that [color].*)
let sum_end_score (board_ext:te) (board:t) (turn:color) (len:int): int =  
  let count = ref 0 in  
  for xt = 0 to len do 
    for yt = 0 to len do
      match board_ext.(xt).(yt) with 
      | Block -> begin
          board.(xt).(yt) <- Empty;
          count := (!count) + 1;
        end
      | Confirmed b -> begin
          board.(xt).(yt) <- Empty;
          count := (!count) + if b then 1 else 0;
        end
      | L l -> board.(xt).(yt) <- l;
    done;
  done;
  !count

(** [end_score] takes as paramters: [color], a board of type [t]
    @return [int] representing the final score for that [color].*)
let end_score (turn:color) (board:t) : int = 
  if count (opposite turn) board = 0 then begin 
    let len = Array.length board in 
    len * len - count turn board
  end else
    let board_extended = convert board in 
    let len = (Array.length board_extended) - 1 in 
    for x = 0 to len do
      for y = 0 to len do
        let ci = {x=x;y=y} in
        if board_extended.(x).(y) = L Empty then begin 
          let b = try is_countable turn ci board_extended with 
            | PropogateTrue -> true
            | PropogateFalse -> false
          in board_extended.(ci.x).(ci.y) <- Confirmed b
        end
      done;
    done;
    sum_end_score board_extended board turn len

let cn_helper (turn:color) (c:coords) (board:t) = 
  if not (bounds c board) then {num_same = 0; num_blank = 0; num_diff = 1} 
  else
    match board.(c.x).(c.y) with 
    | Empty -> {num_same = 0; num_blank = 1; num_diff = 0} 
    | Piece c  when c = turn -> {num_same = 1; num_blank = 0; num_diff = 0} 
    | _ -> {num_same = 0; num_blank = 0; num_diff = 1} 

let sum_data a b c d = 
  {num_same = a.num_same + b.num_same + c.num_same + d.num_same;
   num_blank = a.num_blank + b.num_blank + c.num_blank + d.num_blank;
   num_diff = a.num_diff + b.num_diff + c.num_diff + d.num_diff}

let characterize_neighbors (turn:color) (c:coords) (board:t) : neighbor_data = 
  let right = cn_helper turn {c with x = c.x+1} board in
  let left = cn_helper turn {c with x = c.x-1} board in
  let up = cn_helper turn {c with y = c.y+1} board in
  let down = cn_helper turn {c with y = c.y-1} board in
  sum_data right left up down 

(*Outputs spacers for the board. 
[acc] is the accumulator, size is the board size *)
let rec spacers acc size = 
  match acc with
  |a when a = size -> "  |"
  |b -> "  | " ^ spacers (acc+1) size 

let rec numbers acc size  : string= 
  match acc with
  |x when x = (size + 1) -> "  " ^ string_of_int (size + 1)
  |_ -> if acc = 1 || acc > 10 then 
      ("  " ^ (string_of_int acc) ) ^ numbers (acc+1) size 
    else 
      ("   " ^ (string_of_int acc)) ^ numbers (acc+1) size 



(** loc_description is a dataType used to print the board to the terminal.
    Key: B = bottom, T = top, L = left, R = right, C = corner, 
    V is vertical (column), H is horizontal (row), M is middle, N is none *)
type loc_description = BLC | TLC | BRC | TRC | LV | RV | TH | BH | M | N
(**fancy_printer_dt holds the location of the peice and its label 
   Ex. (TRC,Empty) *)
type fancy_printer_dt = loc_description * location

(** helper function for printing the board *)
let fancy_printer (a:fancy_printer_dt) count = 
  match a with
  |(BLC, Empty) -> if !count < 10 then print_string " " else ();
    print_int !count; count := (!count) + 1; 
    ANSITerminal.print_string [] "└───";
  |(BLC, Piece White) -> if !count < 10 then print_string " " else ();  
    print_int !count; count := (!count) + 1; ANSITerminal.print_string [] "⚪──"
  |(BLC, Piece Black) -> if !count < 10 then print_string " " else ();  
    print_int !count; count := (!count) + 1; ANSITerminal.print_string [] "⚫──"
  |(TRC, Empty) -> ANSITerminal.print_string []  "┐"; print_endline "";
  |(TRC, Piece White) -> ANSITerminal.print_string [] "⚪"; print_endline "";
  |(TRC, Piece Black) -> ANSITerminal.print_string [] "⚫"; print_endline "";
  |(BRC, Empty) -> ANSITerminal.print_string [] "┘"
  |(BRC, Piece White) -> ANSITerminal.print_string [] "⚪"
  |(BRC, Piece Black) -> ANSITerminal.print_string [] "⚫"
  |(TLC, Empty) -> if !count < 10 then print_string " " else ();  
    print_int !count; count := (!count) + 1; ANSITerminal.print_string [] "┌───"
  |(TLC, Piece White) -> if !count < 10 then print_string " " else ();  
    print_int !count; count := (!count) + 1; ANSITerminal.print_string [] "⚪──"
  |(TLC, Piece Black) -> if !count < 10 then print_string " " else ();  
    print_int !count; count := (!count) + 1; ANSITerminal.print_string [] "⚫──"
  |(BH, Empty) -> ANSITerminal.print_string [] "┴───"
  |(BH, Piece White) -> ANSITerminal.print_string [] "⚪──"
  |(BH, Piece Black) -> ANSITerminal.print_string [] "⚫──"
  |(TH, Empty) -> ANSITerminal.print_string [] "┬───"
  |(TH, Piece White) -> ANSITerminal.print_string [] "⚪──"
  |(TH, Piece Black) -> ANSITerminal.print_string [] "⚫──"
  |(LV, Empty) -> if !count < 10 then print_string " " else ();  
    print_int !count; count := (!count) + 1; ANSITerminal.print_string [] "├───"
  |(LV, Piece White) -> if !count < 10 then print_string " " else ();  
    print_int !count; count := (!count) + 1; ANSITerminal.print_string [] "⚪──"
  |(LV, Piece Black) -> if !count < 10 then print_string " " else ();  
    print_int !count; count := (!count) + 1; ANSITerminal.print_string [] "⚫──"
  |(RV, Empty) -> ANSITerminal.print_string [] "│";  print_endline "";
  |(RV, Piece White) -> ANSITerminal.print_string [] "⚪"; print_endline "";
  |(RV, Piece Black) -> ANSITerminal.print_string [] "⚫"; print_endline "";
  |(M, Empty) -> ANSITerminal.print_string [] "┼───"
  |(M, Piece White) -> ANSITerminal.print_string [] "⚪──"
  |(M, Piece Black) -> ANSITerminal.print_string [] "⚫──"
  |_, _ -> failwith "If you somehow got this bug... congrats."

and print_processor (board : t) (len : int) = 

  (** helper function for printing the board *)
  let processed = Array.make_matrix (len + 1) (len + 1) (N, Empty) in 
  for x = 0 to len do 
    for y = 0 to len do 
      if x = 0 && y = 0 then 
        processed.(x).(y) <- (TLC, board.(x).(y))
      else if x = len && y=len then 
        processed.(x).(y) <- (BRC, board.(x).(y))
      else if x = len && y=0 then 
        processed.(x).(y) <- (BLC, board.(x).(y))
      else if x = 0 && y=len then 
        processed.(x).(y) <- (TRC, board.(x).(y))
      else if 0 < x && x < len && y = 0 then
        processed.(x).(y) <- (LV, board.(x).(y))
      else if 0 < x && x < len && y = len then
        processed.(x).(y) <- (RV, board.(x).(y))
      else if x = 0 && 0 < y && y < len then 
        processed.(x).(y) <- (TH, board.(x).(y))
      else if x = len && 0 < y && y < len then 
        processed.(x).(y) <- (BH, board.(x).(y))
      else
        processed.(x).(y) <- (M, board.(x).(y))
    done;
  done;
  processed

(** Prints out the board of type [t] to the console. *)
let print_board (board : t) = begin
  let len = Array.length board - 1 in 
  let processed = print_processor board len in 
  let count = ref 1 in 
  let fancy_printer_itr x = fancy_printer x count in

  let print_arr a count = Array.iter fancy_printer_itr a;
    if (fst a.(0)) <> BLC then 
      print_endline (spacers 0 len) 
    else print_endline "";
  in let print_arr_itr x = print_arr x !count in
  Array.iter print_arr_itr processed; 
  print_string (numbers 1 len);
  print_endline "";
end
