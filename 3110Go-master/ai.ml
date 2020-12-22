open Board
open Command
(** [get_tail lst] returns the tail of a list. lst is a list *)
let rec get_tail lst =
  match lst with 
  |[] -> failwith "Can't find tail of empty list!"
  |h::[] -> h
  |h::t -> get_tail t

(** [first_move state] returns the AI players first move. State is the initial
    game state *)
let first_move (state: game_state) =
  let board_length = Array.length state.board in
  let coordinate = board_length / 3 in 
  Move {x = coordinate; y = coordinate}

(** [heuristic state original_move prisoners_weight territories_weight eye_weight
    connected_weight] returns a float value that represents how close the AI
    player is to winning or losing in a given state. state is the current game 
    state. original_move is the original move from the first level of the 
    game_state tree. prisoners_weight is the constant factor that specifies how 
    much to weight captured prisoners in the heuristic function. 
    territories_weight is the constant factor that specifies how 
    much to weight territories in the heuristic function. eye_weight is the 
    constant factor that specifies how much to weight filling in own eyes in the 
    heuristic function. connected_weight is the constant factor that specifies how 
    much to weight the whether a move is connected to other own pieces on the 
    board in the heuristic function *)
let heuristic state original_move prisoners_weight territories_weight eye_weight
    connected_weight =  
  let players_with_prisoner_scores = state.ps in
  let own_color = 
    if (fst players_with_prisoner_scores).name = "AI" then White 
    else if (snd players_with_prisoner_scores).name = "AI" then Black
    else failwith "No AI player!" in
  let orig_move_coords =
    match original_move with 
    |Pass -> failwith "unimplemented"
    |Move c -> c in
  let move_neighbor_data = 
    characterize_neighbors own_color orig_move_coords state.board in 
  let fill_in_eye = if move_neighbor_data.num_same = 4 then 1. else 0. in
  let connected =  if move_neighbor_data.num_same = 1 then 1. else 0. in
  match own_color with
  |White -> 
    let self =  fst players_with_prisoner_scores in 
    let opponent = snd players_with_prisoner_scores in
    let own_prisoners = self.score in
    let opponents_prisoners = opponent.score in
    let own_territories = 
      if territories_weight = 0. then 0. else
        end_score White state.board |> float_of_int in
    let opponents_territories = 
      if territories_weight = 0. then 0. else
        end_score Black state.board |> float_of_int in
    prisoners_weight *. (own_prisoners -. opponents_prisoners) +. 
    territories_weight *. (own_territories -. opponents_territories)
    -. eye_weight *. fill_in_eye +. connected_weight *. connected
  |Black ->
    let self =  snd players_with_prisoner_scores in 
    let opponent = fst players_with_prisoner_scores in
    let own_prisoners = self.score in
    let opponents_prisoners = opponent.score in
    let own_territories = 
      if territories_weight = 0. then 0. else 
        end_score Black state.board |> float_of_int in
    let opponents_territories = 
      if territories_weight = 0. then 0. else 
        end_score White state.board |> float_of_int in
    prisoners_weight *. (own_prisoners -. opponents_prisoners) +. 
    territories_weight *. (own_territories -. opponents_territories)
    -. eye_weight *. fill_in_eye +. connected_weight *. connected

(** [iterate_y_values state x y acc] is a helper function for generate_move_list
    state is the current state. x and y are ints that represent coordinates on 
    the board. Acc is a accumulator list of coords
*)
let rec iterate_y_values state x y acc =
  if y = Array.length state.board then acc else 
    match is_valid_move state.turn {x = x; y = y} (Board.copy state.board) with
    |Yes prisoners -> iterate_y_values state x (y+1) ({x = x; y = y}::acc)
    |No error -> iterate_y_values state x (y+1) acc

(** [iterate_x_values state x acc] is a helper function *)
let rec iterate_x_values state x acc =
  if x = Array.length state.board then acc else
    iterate_x_values state (x+1) (iterate_y_values state x 0 acc)

(** [generate_move_list state] returns a list of valid moves from a game_state
    state *)
let generate_move_list state = 
  iterate_x_values state 0 []

(** [next_states_aux state move_list acc] is a helper function for next_states*)
let rec next_states_aux state move_list acc =
  match move_list with
  |[] -> acc
  |h::t -> next_states_aux state t ((get_next_state state (Move h))::acc)

(** [next_states state] is a list of all states reachable with valid moves from
    a given game_state state *)
let next_states state = 
  let valid_move_list = generate_move_list state in
  next_states_aux state valid_move_list []

(** [leaf] is the data type that represents a leaf in the game state tree. Each
    leaf contains a game state, a heuristic value, and a list of previous moves 
    to get to that game state *)
type leaf = {
  state: game_state;
  heuristic_val: float;
  previous_moves: move list
}

(** [state_tree] is the data type representing a game state tree *)
type state_tree = 
  | Leaf of leaf
  | Node of node

(** [node] is a data type that represents a node in the game state tree. Each
    node represents a game state in the tree and has a list of state sub trees
    representing game trees for all states reachable from the state *)
and node = { 
  state: game_state; 
  next_states: state_tree list;
}

(** [make_tree_aux depth_bound current_depth prisoners_weight 
    territories_weight eye_weight connected_weight list_of_moves start_state] 
    is a helper function for make_tree*)
let rec make_tree_aux depth_bound current_depth prisoners_weight 
    territories_weight eye_weight connected_weight list_of_moves start_state =
  if current_depth = 0 then
    match next_states start_state with 
    |[] -> 
      failwith "Can't build tree! No valid moves!"
    |next_states ->
      Node {state = start_state; 
            next_states = 
              List.map (make_tree_aux depth_bound (current_depth+1) 
                          prisoners_weight territories_weight eye_weight 
                          connected_weight list_of_moves) next_states}
  else
    match start_state.last_move with 
    |None -> failwith "impossible, last move cannot be None!"
    |Some prev_move-> 
      let updated_move_list = prev_move::list_of_moves in
      if current_depth = depth_bound then 
        begin
          Leaf {state = start_state; 
                heuristic_val = heuristic start_state 
                    (get_tail updated_move_list) prisoners_weight 
                    territories_weight eye_weight connected_weight; 
                previous_moves = updated_move_list} 
        end 
      else
        match next_states start_state with 
        |[] ->
          Leaf {state = start_state; 
                heuristic_val = heuristic start_state 
                    (get_tail updated_move_list) prisoners_weight 
                    territories_weight eye_weight connected_weight ; 
                previous_moves = updated_move_list} 
        |next_states ->
          Node {state = start_state; 
                next_states = 
                  List.map (make_tree_aux depth_bound (current_depth+1) 
                              prisoners_weight territories_weight eye_weight 
                              connected_weight updated_move_list) next_states}

(** [make_tree s d prisoners_weight territories_weight eye_weight 
    connected_weight] returns a game state tree with root s of depth d. 
    The rest of the arguments specify the weights for the heuristic function *)
let make_tree start_state depth prisoners_weight territories_weight eye_weight 
    connected_weight =
  make_tree_aux depth 0 prisoners_weight territories_weight eye_weight 
    connected_weight [] start_state

(** [max_leaf leaf1 leaf2] returns the leaf with the maximum heuristic value out
    of two leaves *)
let max_leaf leaf1 leaf2 =
  if leaf1.heuristic_val > leaf2.heuristic_val then leaf1 else leaf2

(** [max_leaf leaf1 leaf2] returns the leaf with the minimum heuristic value out
    of two leaves *)
let min_leaf leaf1 leaf2 =
  if leaf1.heuristic_val < leaf2.heuristic_val then leaf1 else leaf2

(** [max_leaf_list leaf_list] is the leaf in the leaf list with the maximum 
    heuristic value *)
let rec max_leaf_list leaf_list =
  match leaf_list with
  |[] -> None
  |leaf::t -> 
    match max_leaf_list t with
    |None -> Some leaf
    |Some t_max -> Some (max_leaf leaf t_max)

(** [min_leaf_list leaf_list] is the leaf in the leaf list with the minimum 
    heuristic value *)
let rec min_leaf_list leaf_list =
  match leaf_list with
  |[] -> None
  |leaf::t -> 
    match min_leaf_list t with
    |None -> Some leaf
    |Some t_min -> Some (min_leaf leaf t_min)

(** [max_turn game_tree] is a mutually recursive function that conducts minimax
    search on a game tree. It returns the leaf in the game tree with the maximum
    heuristic value and finds the optimal move from the AI player's perspective  *)
let rec max_turn game_tree =
  match game_tree with
  |Leaf l -> l
  |Node state ->
    match max_leaf_list (List.map min_turn state.next_states) with
    |None -> failwith "Invalid tree. Node must have children!"
    |Some value -> value

(** [min_turn game_tree] is a mutually recursive function that conducts minimax
    search on a game tree. It returns the leaf in the game tree with the minimum
    heuristic value and finds the optimal move from the human opponent's player's 
    perspective  *)
and min_turn game_tree =
  match game_tree with
  | Leaf l -> l
  | Node state ->
    match min_leaf_list (List.map max_turn state.next_states) with
    |None -> failwith "Invalid tree. Node must have children!"
    |Some value -> value

(** [calc_tree_depth board] is the optimal depth of the game tree given the size
    of the board given time and space constraints *)
let calc_tree_depth board =
  let board_length = Array.length board in
  let num_positions = board_length * board_length in
  ((6. *. log 10.) /. (num_positions |> float_of_int |> log)) |> floor
  |> int_of_float

(** [choose_opt_move state tree_depth prisoners_weight territories_weight 
    eye_weight connected_weight] chooses the optimal move using minimax search 
    given a game state and a depth to search *)
let choose_opt_move state tree_depth prisoners_weight territories_weight 
    eye_weight connected_weight =
  let tree = make_tree state tree_depth prisoners_weight territories_weight 
      eye_weight connected_weight in
  let leaf = max_turn tree in
  let move_list = leaf.previous_moves in
  get_tail move_list

(** [choose_opt_move_with_opt_depth state prisoners_weight territories_weight 
    eye_weight connected_weight] chooses the optimal move using minimax search 
    given a game state. It automatically calculates the optimal depth to search
*)
let choose_opt_move_with_opt_depth state prisoners_weight territories_weight 
    eye_weight connected_weight =
  let tree_depth = calc_tree_depth state.board in
  let tree = make_tree state tree_depth prisoners_weight territories_weight 
      eye_weight connected_weight in
  let leaf = max_turn tree in
  let move_list = leaf.previous_moves in
  get_tail move_list

