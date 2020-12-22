(**
    Representation of game board for Go

    This module represents the board with the pieces on it.  
    It is changed by moves, and can determine whether or not moves are correct.
*) 
(** The abstract type representing the color of a piece *)
type color = White | Black

type neighbor_data = {num_same : int; num_blank : int; num_diff : int}

(** The abstract type represeting the contents of a location on the board  *)
type location = 
  | Empty
  | Piece of color 

(** The abstract type representing a game board *)
type t = location array array

(** The abstract type representing a move *)
type valid_move = 
  |Yes of int
  |No of string 

(** Holds both the board and the score*)
type game_data = {board: t; score: int;}  

(** The coordinates on the board *)
type coords = {x : int; y : int}

exception MoveError of string

(** A function that returns the opposite color of the input*)
val opposite: color -> color

(** A function that creates a new (empty) game board of a given size *)
val init : int -> t

(** A function that returns true iff a game board is only empty spaces*)
val is_empty : t -> bool

(** A function that copies a game board *)
val copy : t -> t

(** A function to try to place a piece at a given location *)
val move : color -> coords -> t -> game_data 

(** A function that determines if a move is valid. 
 *  If invalid, throws an exception with the reason why 
    Returns: -1 if invalid move, otherwise int >=0 for how many
    peices captured if any*) 
val is_valid_move : color -> coords -> t -> valid_move

(** This function takes in a coord and then determines how many of the 
    surrounding spaces are blank, the same color, diffrent color.  Then it 
    stores it in the [neighbor_data] datatype. *)
val characterize_neighbors : color -> coords -> t -> neighbor_data 

(** The points that the given color gains at the end of the game from its 
 * territories (does not include prisoners), according to the rules of Go *)
val end_score : color -> t -> int

(** Prints out the board to the output stream *)
val print_board: t -> unit
