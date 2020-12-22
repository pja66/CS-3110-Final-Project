(**
    Representation of game board for Go
    This module represents the CLI interface to play the game. 
*) 
open Board 
open Printf
open Command

(** The abstract type representing the players.*)
type gameType = AI |TwoPlayer

(** Function's purpose is to retrieve keyboard inputs without clicking enter, 
    allows for a much more user experience.  *)
val get1char: unit -> char

(**Function creates a new player *)
val player_creator: gameType -> Command.player * Command.player

(**Polls the user for a specif board size. *)
val board_size: unit -> int

(**Queues up start screen and polls the user for info. *)
val main_menu: unit -> gameType

(**Gets a particular players move *)
val get_move: Command.game -> unit -> unit

(**Calls all the visual function so the users can play the game.*)
val main: unit -> unit
