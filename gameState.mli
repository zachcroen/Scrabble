(**
    Representation of static game data (including rules, specific game modes). 

    This includes parsing game state configuration from a .json file. 
    The game configuration currently consists of a set of tiles, and board size.
*)
(** exceptions for GameState *)
exception EmptyBag
exception BadJson
exception TilesNotinRack

(** [plyr] is the type representing a player. Has a name, score, and 
    tile rack. 
    AF: [{name = s; score = c, tile_rack = t}] represents a player 
        whose name is [s], whose score is [c], and whose current "hand" of 
        tiles (i.e. their tile rack) has the tiles in [t]. 
    RI: 
    - [score] is a non-negative integer (unless the game is ending and 
        remaining tiles are being subtracted). 
    - length of [tile_rack] is no greater than 7. *)
type plyr = {
  name : string;
  score : int;
  tile_rack : Scrabble.tile list;
}

(** [t] is type representing a GameState, 
    declared across the interface for testing. 
    AF: [{ 
        bag = b; board = br; players = plst; turn_number = c;
        }] represents the stage in a scrabble game where the bag has the 
        tiles specified by [b], the board looks like [br], the players in the 
        game have the characteristics specified by [plst], it is currently 
        the [c]th round of play. 
    RI: 
    - [turn_number] is a non-negative integer (unless the game is ending, 
        indicated by setting [turn_number] to [-1]. 
    - the players in [players] all have distinct names. *)
type t = {
  bag : Scrabble.tile list;
  board : Scrabble.board;
  players : plyr list;
  turn_number : int;
}

(** [get_start s] are the coordinates of the start tile for the board 
    in state [s]. *)
val get_start : t -> int * int 

(** [shuffle_bag t] is state [t] but with the tile bag shuffled (i.e. 
    tiles put in randomized order. *)
val shuffle_bag : t -> t

(** [num_letters_left st] is how many letters remain in the tile bag 
    in state [st].  *)
val num_letters_left : t -> int 

(** [get_letter st] is a tile from the tile bag in [st]. If the bag is 
    empty, it is [None]. *)
val get_letter : t -> Scrabble.tile option

(**[from_json j] is the scrabble game that [j] represents.
   RI: [j] is a valid JSON scrabble representation: 
   - Must specify a starting coordinate 
   - Must have enough tiles for the number of players you wish to include 
            (i.e. 7 * number of players).
   - Must specify a board size 
   - All tiles must have all three fields: letter, points, and count. 
     Raises: [BadJson] if [j] is not a valid json file for use. *)
val from_json : Yojson.Basic.t -> t

(** [remove_from_bag t] is state [t] but wihtout the "first" letter in the tile 
    bag of [t] 
    Raises: [EmptyBag] if the tile bag of [t] is empty. *)
val remove_front_tile : t -> t 

(** [add_players t p] is [t] with player [p] added to the player list. *)
val add_player : t -> plyr -> t 

(** [update_score state name score] is state [state] but with [score] replacing  
    the existing score of player [name] in [state]. 
    Requires: player [name] is a player in the game represented by [state]. *)
val update_score : t -> string -> int -> t

(** [assign_pts state player tlst] is a tile list with all of the same 
    letters as [tlst], but with the proper points assigned to each letter.
    Uses points in [player]'s rack in [state] to obtain proper
    point values. *)
val assign_pts : t -> string -> Scrabble.tile list -> Scrabble.tile list

(** [chck_rck s name tlst] is the state [s] if all of the tiles in the 
    tile list [tlst] are in player [name]'s rack in state [s]. If not, raises 
    [TilesNotinRack]. *)
val chck_rck : t -> string -> Scrabble.tile list -> t

(** [draw_tiles state num] is a pair. The first entry is the 
    same as [state], but with [num] tiles removed from the bag. The second 
    entry is a list of [Scrabble.tiles] with the tiles that have been drawn from 
    the bag in [state]. *)
val draw_tiles : t -> int -> t * Scrabble.tile list

(** [get_player state name] is the player in [state] with name [name]. 
    Requires: player [name] is a player in the game represented by [state].*)
val get_player : t -> string -> plyr

(** [whose_turn state] is the player whose turn it is to play, according to the 
    turn number in [state]. 
    Requires: turn number in [state] is >= 0. *)
val whose_turn : t -> plyr

(** [inc_turn state] is [state], but with the turn number increased by 1. *)
val inc_turn : t -> t

(**[pass_turn state] is the same state as [state], but where it is now 
   the next player's turn to play (without changing turn number). *)
val pass_turn : t -> t

(* [go_to_new_state state tlst coors d size] is a new state that reflects the 
   executes the following steps:
   - Add the word specified by [tlst] in the direction [d] at starting 
     coordinate [coors] to the board in [state] (which has size [size]). 
   - Compute the points of that play and add them to the points in [state] of 
     the player whose turn it is. 
   - Remove the tiles in [tlst] from the player's rack in [state]. Draw 
     [List.length tlst] new tiles from the tile bag in [state], and add them 
     to the player's tile rack. 
   - Increment the turn number in [state] by 1. *)
val go_to_new_state : 
  t -> Scrabble.tile list -> int * int -> string -> int -> t 

(** [swap_tiles t lst] moves to the next players turn, after exchanging the 
    letters in [lst] with the tile bag in [t] *)
val swap_tiles : t -> Scrabble.tile list -> t

(** [end_game st] is the same state as [st], with the turn number set to 
    -1 to indicate that the game is finished. [end_game] also computes the 
    final scores of each player by doing the following: if a player's 
    tile rack is empty, his/her total score is his/her final score. If the 
    player still has tiles on his/her rack, subtract the total points of the 
    remaining tiles from their his/her total score, and this becomes the final 
    score. [end_game] prints the final scores. *)
val end_game : t -> t