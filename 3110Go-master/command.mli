open Board 
(**
    Representation of players for Go.

    This module represents everything that is not held in the board module
    such as players, turn, game state, past moves, and undo/redo functionality.
*) 

(** The abstract type representing a player.*)
type player ={name: string; score: float} 
(** The abstract type representing a both players.*)
type players = (player*player)
(** The abstract type representing a move.*)
type move = 
  | Move of coords
  | Pass

(** The abstract type representing an entire game.*)
type game = {past: game_state list; future: game_state list}

(** The abstract type representing an the state of the game.*)
and game_state = {board: Board.t; 
                  ps: players; 
                  turn: Board.color; 
                  last_move: move option}


exception GameEnd
exception KohViolated

(**  Creates a player whose name is the given string and whose 
 * score is the given float *)
val create_player: string -> float -> player 

(** Creates a new game between the two given players, where the first is white
 * and the second is black.  The board is blank with the size being the 
 * given int.  It is the first player's (white's) turn to move. *)
val start_game: int -> player -> player -> game

(**  Makes the move specified, whether it is a pass or an idea to place
 *  a piece at the given coordinates.  Returns a game value that has
 *  been updated to reflect the move.  Throws a GameEnd exception if the 
 *  move is a consecutive pass, or propogates a MoveError exception if the 
 *  given move is not valid *)
val make_move: game -> move -> game 

(** Returns a game state that has been updated to add the two players' scores
 * earned using the game-ed scoring in Go.  The board will be unchanged. *)
val end_game: game -> game

(** Returns the string of the player to act ("White" or "Black") *)
val turn: game -> string

(** Prints out who won the game.  Assumes end_game has been called already *)
val winner: game -> unit

(*Prints out the game board to the command line *)
val print_game : game -> unit

(** Returns current game state from game data type *)
val get_curr: game -> game_state

(** get_next_state m s returns the game_state after applying move m to game_state
    s *)
val get_next_state: game_state -> move -> game_state