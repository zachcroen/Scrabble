(** 
   The main entry point for the game interface.
*)

(** [execute_command c state pass_cnt] is a pair (g, p). g is the new GameState 
    resulting from executing command [c] when at state [state]. If [c] is an 
    illegal command, then the user is prompted to enter a new command, 
    [c'], and [execute command c' state pass_cnt] is called. [p] is a 
    count of the number of consecutive times players have passed: 
    if [c] results in a passing of turn, then [p] = [pass_cnt + 1]. *)
val execute_command : string -> GameState.t -> int -> GameState.t * int

(** [keep_playing state pass_cnt] plays the game specified by state [state] 
    according to commands given by the user. [pass_cnt] is the number of
    consecutive times players have passed their turns (i.e. if on the 
    first 5 turns, the players pass, then [pass_cnt] = 5. *)
val keep_playing : GameState.t -> int -> unit

(** [start_scrabble file] starts the game specified by file name [file]. *)
val start_scrabble : string -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit

