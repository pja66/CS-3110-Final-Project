open Board
open Command

(** This function takes in a game state and returns a float that represents how 
    close or far away the AI player is to winning the game in the given state. This
    function is used in the minimax search algorithm to rank moves and ultimately
    find the optimal move for the AI player at certain point in the game. The
    function also takes in the original move from the first level of the game 
    state tree 
*)
val heuristic: Command.game_state -> Command.move -> float -> float -> float -> 
  float -> float


(** This function generates a list of all possible moves on the board given a 
    game state *)
val generate_move_list: Command.game_state -> Board.coords list

(** This function uses minimax search to find the optimal move for the AI player
*)
val choose_opt_move: Command.game_state -> int -> float -> float -> float -> 
  float -> Command.move

(** This function uses minimax search to find the optimal move for the AI player 
    and calculates automatically what the depth of the game state tree should be
    given the size of the board *)
val choose_opt_move_with_opt_depth: Command.game_state -> float -> float -> 
  float -> float -> Command.move

(** This function determines what move the AI player should make on its first 
    move. Because it is easier to secure territory in the corners of the board, 
    the AI player makes a move near one of the star points on a traditional Go 
    board *)
val first_move: Command.game_state -> Command.move

(** A leaf is the data type that represents a leaf in the game state tree. 
    Each leaf represents a game state that is reached from making all of the
    moves in the previous_moves list from the current state of the game. Each 
    leaf has an associated heuristic value that minimax search uses to find the 
    minimum or maximum leaf *)
type leaf = {
  state: game_state;
  heuristic_val: float;
  previous_moves: move list
}

(** state_tree is a tree of game states that is either a leaf or a node *)
type state_tree = 
  | Leaf of leaf
  | Node of node

(** node is a data type that represents a node in the gamestate tree. It has
    a corresponding game state and a list of next states which consists of all 
    game_states that can be reached from making valid moves from the game state
    of the node *)
and node = { 
  state: game_state; 
  next_states: state_tree list;
}

(** This function builds the game state tree for a given current game state *)
val make_tree: Command.game_state -> int -> float -> float -> float -> float -> 
  state_tree

(** This function produces all the game states that can be reached from a given
    game state*)
val next_states: Command.game_state -> Command.game_state list

(** This function outputs the maximum of two leaves compared by their heuristic 
    values *)
val min_leaf: leaf -> leaf -> leaf

(** This function outputs the minimum of two leaves compared by their heuristic 
    values *)
val max_leaf: leaf -> leaf -> leaf
