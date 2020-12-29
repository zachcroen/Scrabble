(** Module for interpreting user inputted commands to the game.*)

(**********************************************************)

(** [MalformedCommand] is thrown when the user inputs a command that 
    does not follow the formatting rules of the parser.   *)
exception MalformedCommand

(** [EmptyCommand] is thrown when the user inputs the empty string as a
    command to the parser. *)
exception EmptyCommand

(** [play_move] represents the components that are associated with the act 
    of giving a command to play a word onto the scrabble board.   *)
type play_move =  {tiles : Scrabble.tile list;
                   location : (int * int);
                   direction : string}

(** [command] represents the interpreted string from the parser that is fed
    to the rest of the game *)
type command = 
  | Play of play_move
  | Swap of Scrabble.tile list
  | Help
  | Quit
  | Pass  

(** [parser s] parses [s] and returns a command. *)
val parser : string -> command