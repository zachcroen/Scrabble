(** Representation of static Scrabble board.*)

(**********************************************************)


(** The abstract type representing a Scrabble board. Board [b] represents a 
    Scrabble board with particular characteristics, 
    including the number and locations of spaces, the special attributes, if 
    any, of each space, and the tiles, if any placed on each space. *)
type board 

(** Type of tile letters. Char ['c'] represents the letter c. 
    ['c'] is 'A',...,'Z', or '_'. *)
type letter = char 

(** Type of tile points. Int [n] is [n] points. *)
type points = int 

(** The abstract type representing a tile 
    AF: [{l = c; p = n}] is the Scrabble tile with letter [s] woth [n] 
    points. *)
type tile = {
  l : letter;
  p : points
}

(** AF: The type of special attributes for spots on a scrabble board. 
    One of: Double word, triple word, double letter, triple letter, or 
    starting spot. All of these attributes affect the score of a play 
    in the natural way (i.e. double word doubles the score of the word, etc.).
    Starting spot denotes the required starting space for that Scrabble board, 
    and also acts as the double word attribute. *)
type specials

(** Type of spot specifications (data for a given spot). A spot specification
    tells what tile is on that spot, if any, and whether or not that 
    spot has any special properties (double letter, triple word, etc.) *)
type spot_specs = {
  tile : tile option;
  special : specials;
}

(** Raised when a word is attempted to be placed in a direciton that is not
    "across" or "down."*)
exception UnknownDirection of string

(** Raised when a tile is attempted to be placed at coordinates that are 
    not on the board. *)
exception BadCoordinates of int * int 

(** Raised when a word is attempted to be placed such that: 
    - One or more tiles are in a spot that already contains a tile 
      OR 
    - No tiles are adjacent to spots which already contain tiles. *)
exception InvalidMove

(** Raised when a word is attempted to be played that is not in the
    dictionary, and thus not a valid word to be placed*)
exception InvalidWord of string

(** [specials s] is the string representing the special attribute 
    [s]. *)
val special_tostr : specials -> string 

(** [board_length b] is the dimension of the board, which must be 
    square (i.e. for a 9x9 board [b], [board_length b] is 9. *)
val board_length : board -> int

(** [get_spot_from_board b r c] is the spot_specs of location ([r],[c]) on 
    board [b] *)
val get_spot_from_board : board -> int -> int -> spot_specs

(** [get_start b] is the starting coordinate of board [b]. 
    Raises: [Not_found] if [b] does not have a starting coordinate. *)
val get_start : board -> int * int

(** [get_tile_from_board b r c] is the tile in row [r] and column [c] in 
    board [b], if there is one (if not, it is "None"). 
    Requires: if [b] is n x n, then [r] and [c] are <= n. *)
val get_tile_from_board : board -> int -> int -> tile option

(** [make_empty_board n dl tl dw tw st] is an empty n x n Scrabble board 
    with each tile set to be empty, and special spaces corresponding to 
    lists [dl], [tl], [dw], [tw], [st] which represent double letter, triple 
    letters, double words, triple words, and the starting space.
    Requires: [st] has exactly one element. *)
val make_empty_board : int -> (int * int) list -> (int * int) list -> 
  (int * int) list -> (int * int) list -> (int * int) list -> board 

(** [add_word t d s b bool] adds the tiles in the char list [t] to 
    board [b] in the direction [d] (either top to bottom or left to 
    right). Tiles begin to be placed at coorinate [s] where s = (row, col). 
    Note that a tile with letter set to ' ' indicates that the letter
    needed here is already on the board. The function skips over this "tile"
    without adding it to the board, and it moves to the next space. 
    Example: If the 3 x 3 board is 
      [ _ , _ , D]
      [ _ , _ , O]
      [ _ , _ , _], 
      then adding [{l = ' ', p = 0}; {l = ' ', p = 0}; {l = 'G'; p = 5}]
      in direction "down" starting at coordinate (1,3) gives:
       [ _ , _ , D]
       [ _ , _ , O]
       [ _ , _ , G] *)
val add_word : tile list -> string -> int * int -> board -> bool -> board 

(** [tile_opt_printer t] is a string representing tile option [t]. 
    It is "None" if [t] is None. *)
val tile_opt_printer : tile option -> string

(**[tile_lst_to_string lst] returns a string of the letters within the lst. 
   Intentded to be used for converting words to strings for lookup *)
val tile_lst_to_string : tile list -> string 

(** [board_printer board] is the printed visual depiction of the board. 
    Prints underscores in place of empty tiles. This includes colors. 
    Requires: board is a square board. *)
val board_printer2 : board -> unit

(** [valid_move t b] raises exceptions if a word [t] is played in an invalid
    way on board [b].  
    (**[valid_move t d s b bool] raises exceptions if a word [t], 
    played in direction [d], at coordinate [s] is played in an invalid way 
    on board [b].  bool is true if it is the first move of the game 
    Raises: InvalidMove -> word attempts to overwrite Some tiles on board
           InvalidMove -> disconnected word from rest of setup
           UnknownDirection -> direction command is not "down" or "across"
           BadCoordinates -> attempts to place tile outside of board dimensions.
    There is no return in the successful case. *) *)
val valid_move : tile list -> string -> int * int -> board -> bool -> unit 

(** [word_in_dict t b s d] is true if all of the words formed by 
    playing [t] on board [b] at start coordinates [s] in direction [d] 
    are in the dictionary specified by [dict.csv]. If not, raises
    [InvalidWord]. *)
val word_in_dict : tile list -> board -> int * int -> string -> bool

(** [score_word t d s b] scores the play that results by adding the word 
    specified by the tile list [t] to board [b] in direction [d] 
    starting at coordinate [s]. 
    Requires: This word must have been, legally, added to the board 
    before calling [score_word] on it. *)
val score_word : tile list -> string -> int * int -> board -> int -> int 


(** [elim_specials board] returns the same board as [board], except 
    that all spaces with a tile on them no longer have "special" 
    qualities (triple word score, etc.). *)
val elim_specials : board -> board 
