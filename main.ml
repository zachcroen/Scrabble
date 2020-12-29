open GameState


(* [broken_file f] is the JSON data given in file [f], of the user's 
   specification. If [f] can not be found, then the user is prompted to 
   input a new file.*)
let rec broken_file f = try (Yojson.Basic.from_file f) with 
  | Sys_error _ -> print_string ("I'm sorry, but I \
                                  can't find that file. Please check that \
                                  the pathname is correct and try again.\n\n"); 
    print_endline "Please enter the name of the game file you want to load \
                   (do not use quotations).\n";
    print_string  "> ";
    match read_line () with
    | file_name -> broken_file file_name

(* [print_exlist e] is the string representation of the string list [e]. *)
let print_exlist exlist = 
  let rec helper e s = match e with 
    | [] -> s
    | h :: t -> if s = "" then helper t (h ^ s) else helper t (h ^ ", " ^ s)
  in 
  helper exlist ""

(* [try_c c state] is the command specified by string [c], input by the user,
   when at state [state]. If [c] is empty or malformed, the user is prompted 
   to enter a new string. *)
let rec try_c c state = try (Command.parser c) with 
  | Command.EmptyCommand -> begin
      print_string ("You have entered an empty command. \
                     Please enter a valid command. \n\n");
      print_string ("> ");
      match read_line () with 
      | command -> try_c command state 
    end 
  | Command.MalformedCommand-> begin 
      print_string ("You have entered a malformed command. \
                     Please enter a valid command. \n\n");
      print_string ("> ");
      match read_line () with 
      | command -> try_c command state 
    end 


(* [empty_bag] is a string input by the user. *)
let empty_bag () = print_string ("The bag is empty. If you can not make a \
                                  play, please pass your turn. \n
                                  Please try a different command.\n\n");
  print_string ("> ");
  match read_line () with  
  | command -> command


(* [handle_badjson] is a string input by the user. *)
let handle_badjson () = print_string ("The json string is bad. \
                                       Please try a different command.\n\n");
  print_string ("> ");
  match read_line () with  
  | command -> command


(* [handle_tiles] is a string input by the user. *)
let handle_tiles () = print_string ("The tiles needed are not in your tile \
                                     rack. Please try a different command.\n
                                     Make sure to capitalize letters, unless \
                                     using a blank.
                                     \n\n");
  print_string ("> ");
  match read_line () with  
  | command -> command


(* [unknown_direction] is a string input by the user. *)
let unknown_direction () = print_string ("Your direction needs to be across \
                                          or down Please try a different \
                                          command.\n\n");
  print_string ("> ");
  match read_line () with  
  | command -> command


(* [bad_coor] is a string input by the user. *)
let bad_coor () = print_string ("Your coordinates are invalid. \
                                 Please try a different command.\n\n");
  print_string ("> ");
  match read_line () with  
  | command -> command


(* [invalid_move] is a string input by the user. *)
let invalid_move () = print_string ("The Placement of your tiles is not \
                                     allowed. Please try a different command.\
                                     \n\n");
  print_string ("> ");
  match read_line () with  
  | command -> command

(* [invalid_word] is a string input by the user. *)
let invalid_word s = 
  print_string (s ^ " is not a word. Please try a different command.\n\n"); 
  print_string ("> ");
  match read_line () with  
  | command -> command

(* [unexpected_error] is triggered when the program fails,
   due to an unknown issue *)
let unexpected_error () = print_string ("You've raised an unanticipated error! \
                                         Please enter a new command \n\n");
  print_string ("> ");
  match read_line () with  
  | command -> command

(* [print_instructions] prints instructions for how to play the game *)
let print_instructions () = 
  ANSITerminal.(print_string [blue] ("\n Example Commands: \n\n"));
  ANSITerminal.(print_string [magenta] ("Play WORD (8,8) down"));
  print_string (" --> plays tiles W-O-R-D starting on tile 8,8 going \
                 down. \n");
  ANSITerminal.(print_string [magenta] ("Play _ILD (8,8) across"));
  print_string (" --> builds the word W-I-L-D off of the existing 'w' across. \
                 \n");
  ANSITerminal.(print_string [magenta] ("Play CoMPUTe_ (8,8) down"));
  print_string (" --> builds the word C-O-M-P-U-T-E-D using the existing \
                 'D' from 'WILD.' Uses a blank tile for the 'O' and a blank \
                 tile for the 'E'.\n");
  ANSITerminal.(print_string [magenta] ("Swap ABCD"));
  print_string (" --> removes letters A-B-C-D from players board, and replaces \
                 an equal number \n");
  ANSITerminal.(print_string [magenta] ("Pass"));
  print_string (" --> skips the players turn. \n");
  ANSITerminal.(print_string [magenta] ("Quit"));
  print_string (" --> exits the game\n\n");
  ANSITerminal.(print_string [blue] ("Board Coloring: \n\n"));
  ANSITerminal.(print_string [cyan] "_ (Cyan) ");
  print_string (" --> Double Letter Score\n");
  ANSITerminal.(print_string [green] "_ (Green) ");
  print_string (" --> Triple Letter Score\n");
  ANSITerminal.(print_string [magenta] "_ (Pink) ");
  print_string (" --> Double Word Score\n");
  ANSITerminal.(print_string [red] "_ (Red) ");
  print_string (" --> Triple Word Score\n");
  ANSITerminal.(print_string [yellow] "_ (Yellow) ");
  print_string (" --> Start Tile (double word score)\n");

  print_string ("> ");
  match read_line () with  
  | command -> command


let rec execute_command c (state : GameState.t) pass_cnt = 
  if state.turn_number = -1 then Stdlib.exit 0 else 
  if state.bag = [] && List.length state.players = pass_cnt 
  then execute_command "pass" (GameState.end_game state) pass_cnt 
  else handle_others c state pass_cnt 

and handle_others c state pass_cnt = 
  match (try_c c state) with 
  | Command.Quit -> begin
      ANSITerminal.(print_string [blue] ("Sorry to see you go! \
                                          The game is now terminating...\
                                          \n\n"));
      Stdlib.exit 0
    end
  | Command.Pass -> GameState.pass_turn state, (pass_cnt + 1)
  | Command.Help -> execute_command (print_instructions ()) state pass_cnt
  | Command.Swap lst -> begin 
      try GameState.swap_tiles state lst, pass_cnt with
      | GameState.TilesNotinRack -> 
        execute_command (handle_tiles ()) state pass_cnt
      | _ -> 
        execute_command (unexpected_error ()) state pass_cnt
    end 
  | Command.Play play_move -> begin
      try GameState.go_to_new_state state 
            (GameState.assign_pts state 
               ((GameState.whose_turn state).name) play_move.tiles) 
            play_move.location play_move.direction 
            (Scrabble.board_length state.board), pass_cnt 
      with 
      | GameState.EmptyBag -> 
        execute_command (empty_bag ()) state pass_cnt
      | GameState.BadJson -> 
        execute_command (handle_badjson ()) state pass_cnt
      | GameState.TilesNotinRack -> 
        execute_command (handle_tiles ()) state pass_cnt
      | Scrabble.UnknownDirection string -> 
        execute_command (unknown_direction ()) state pass_cnt
      | Scrabble.BadCoordinates (x, y) -> 
        execute_command (bad_coor ()) state pass_cnt
      | Scrabble.InvalidMove -> 
        execute_command (invalid_move ()) state pass_cnt
      | Scrabble.InvalidWord s -> 
        execute_command(invalid_word s) state pass_cnt
    end 


(*helper for print tile rack *)
let rec print_letters (tile_rack : Scrabble.tile list) : unit = 
  match tile_rack with 
  | [] -> print_string ("|"); ()
  | h :: t -> begin 
      let str_let = Char.escaped h.l in 
      print_string( "| " ^ str_let ^ " "  );
      print_letters t
    end

(*helper for print tile rack *)
let rec print_points (tile_rack : Scrabble.tile list) : unit = 
  match tile_rack with 
  | [] -> print_string ("|"); ()
  | h :: t -> begin 
      let str_let = string_of_int h.p in 
      print_string( "| " ^ str_let ^ " "  );
      print_points t
    end

(*helper for print tile rack *)
let print_border count : unit = 
  let slash_str = String.make count '-' in 
  print_string ( slash_str )

(*prints players tile rack in an easy to read format*)
let print_tile_rack tile_rack = 
  let border_length = List.length tile_rack * 4 + 1 in 
  (*for top divider*)
  print_string ("         ");
  print_border border_length;

  (*for row of letters*)
  ANSITerminal.(print_string [blue] "\nLetters: ");
  print_letters tile_rack;

  (* for middle divider *)
  print_string ("\n         ");
  print_border border_length;

  (*for row of points*)
  ANSITerminal.(print_string [blue] "\nPoints:  ");
  print_points tile_rack;

  (* for bottom border *)
  print_string ("\n         ");
  print_border (border_length); ()


let rec keep_playing (state : GameState.t) pass_cnt = 
  if state.turn_number = -1 then Stdlib.exit 0 else 
  if state.bag = [] && List.length state.players = pass_cnt then 
    let do_pass = execute_command "pass" (GameState.end_game state) pass_cnt in
    keep_playing (fst do_pass) (snd do_pass)
  else not_ending state pass_cnt 

and not_ending state pass_cnt =  
  print_string ("\n\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n");
  Scrabble.board_printer2 state.board;
  print_string ((GameState.whose_turn state).name ^ "\n");
  ANSITerminal.(print_string [green] "score: ");
  print_string ((string_of_int (GameState.whose_turn state).score));
  print_string ("\n");
  print_tile_rack (GameState.whose_turn state).tile_rack;
  print_string ("\nPlease enter a valid move: \n");
  print_string ("-> To use a blank (/) tile, type the lowercase version of 
        the letter for which the blank stands in.");
  if state.turn_number = 0 then begin 
    print_string ("\n-> First word must be played from starting spot: ");
    let start = GameState.get_start state in 
    ANSITerminal.(print_string [yellow] 
                    ("(" ^ (string_of_int (fst start)) ^ "," ^ 
                     (string_of_int (fst start)) ^ ")")); 
  end 
  else (); 
  print_string ("\n-> For instructions, type"); 
  ANSITerminal.(print_string [magenta] (" help"));
  print_string ("\n\n>");
  match read_line () with
  | input -> 
    let res = execute_command input state pass_cnt in 
    keep_playing (fst res) (snd res)

(** [make_player state player] is a tuple of the GameState and the new Player
    created. *)
let make_player state (player : string) : (GameState.t * GameState.plyr) =
  let new_state_player = GameState.draw_tiles state 7 in
  let new_state = fst new_state_player in
  (new_state,
   {
     name = player;
     score = 0;
     tile_rack = snd new_state_player
   })


(** [create_players state players] is the new GameState created when the 
    new players are made and added to the list of players of scrabble. *)
let rec create_players state players : GameState.t=
  match players with
  | [] -> state
  | h :: t -> 
    let new_state_player = make_player state h in
    let new_state = fst new_state_player in
    let player = snd new_state_player in
    create_players 
      {
        bag = new_state.bag;
        board = new_state.board;
        players = player :: new_state.players;
        turn_number = new_state.turn_number;
      } t


let start_scrabble file = 
  let newfile = String.split_on_char ',' file in
  match newfile with
  | [] -> failwith "impossible"
  | h :: t -> 
    let json = h in
    let players = t in
    let game1 = json |> broken_file |> GameState.from_json in 
    let state = create_players game1 players in
    keep_playing state 0


let main () =
  ANSITerminal.(print_string [magenta]
                  "\n\nWelcome to the 3110 Scrabble. \n");
  print_endline "Please enter a list of: the scrabble game that you want \n\
                 to load, followed by player names, all separated by commas.\n\
                 (Ex: standard_game.json, Sophie, Gavin, Zach)\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name  -> start_scrabble file_name

(* Execute the game engine. *)
let () = main () 
