open OUnit2
open Scrabble 
open GameState
open Command 

(*********************************************************
    OVERALL TESTING STRATEGY COMMENT: Our overarching testin strategy best 
    mimicks that from a2. Because our project consists of a playable game, 
    there are elements of the code that is not condusive to OUnit testing. 
    Foremost, it was not really possible to test the [Main] engine via 
    OUnit testing. Instead, we tested our [Main] module through game play
    (of which we did lots!). Additionally, once our game was functional 
    for play in the terminal, simply playing it was a suprisingly useful in 
    tracking down bugs in other parts of our code. Sometimes, simply 
    playing the game would reveal edge cases that we hadn't covered in our 
    OUnit suite— for example, an unexpectedly large increase in score might 
    indicate an unhandled edge case in the scoring section of [Scrabble]. 
    This allowed us to both correct the errors and add appropriate cases to 
    the OUnit suite to ensure that future changes did not trigger the same 
    "hidden" errors. 

    Of course, especially during the early phases of our game development, 
    we relied on OUnit testing and TDD. As we have learned to do throughout 
    the semester, we would write down simple, failing test cases before we 
    started working on code in the [Scrabble] and [Command] modules 
    (and to some extent, we did this for [GameState], although likely [Main], 
    [GameState] has some functionality that is not condusive to OUnit testing 
    and is better explored through game play). Then, as we developed more 
    functionality, we added more test cases and ensured that they passed, and
    so on. This TDD process helped us ensure that we had test cases convering 
    all of the appropriate functionality, and also guided our code development. 
    It is also worth noting that some individual test cases cover several 
    functions at once. For example, by running tests on [st3] in the testing 
    of [GameState], we implicitly ensure that passing a turn, adding a word, 
    scoring a word, etc. work correctly. 

    This test suite provides confidence in the correctness of our algorithm. 
    We employed bisect coverage to ensure that we weren't missing 
    significant chunks of OUnit-testable code. Indeed, our coverage for 
    [Command] and [Scrabble] (ignoring untestable and unreachable regions 
    of code, like the board printer in [Scrabble] and branches of match 
    statements included only for exhaustion) is nearly perfect. Our 
    coverage for [GameState] and [Main] is less good, but, as aforementioned, 
    we verified the correctness of these modules using game play. 
 *********************************************************)


(* Testing helper functions *)

let rec print_tlst l = match l with 
  | [] -> "" 
  | h :: t -> 
    "[" ^ Char.escaped h.l ^ " : " ^ string_of_int h.p ^ "], " ^ (print_tlst t)

let print_toption t = match t with 
  | None -> "None"
  | Some f -> "[" ^ Char.escaped f.l ^ " , " ^ string_of_int f.p ^ "]"


let print_specs (s : Scrabble.spot_specs) = 
  "{" ^ "tile: " ^ print_toption s.tile ^ "special: " ^ 
  Scrabble.special_tostr s.special ^ "}"


(* [check_tile_at_spot n b r c e] tests that the tile at spot ([r], [c]) in 
   board [b] is [e]. Test name is [n]. *)
let check_tile_at_spot
    (name : string)
    (board : board)
    (row : int)
    (col : int) 
    (expected : tile option) =
  name >:: (fun _ -> 
      assert_equal expected (Scrabble.get_tile_from_board board row col)
        ~printer:Scrabble.tile_opt_printer) 

let make_valid_move_test 
    (name : string)
    (word : tile list)
    (direction : string)
    (start_coor : int * int)
    (board : board)
    (bool : bool) : test = 
  name >:: (fun _ -> 
      assert_equal () 
        (Scrabble.valid_move word direction start_coor board bool))

let score_test 
    (name : string) 
    (t_lst : tile list)
    (direction : string) 
    (s : int * int)
    (board : board)
    (dim : int) 
    (exp : int) = 
  name >:: (fun _ -> 
      assert_equal exp (Scrabble.score_word t_lst direction s board dim) 
        ~printer:string_of_int)

(* [fails_well n f func] checks that the function call [func] raises [f]. 
   Test has name [n].  *)
let fails_well 
    (name : string) 
    (fail_type) 
    (func_call) =
  name >:: (fun _ -> assert_raises fail_type func_call)

let special_test 
    (name : string)
    (expected : string)
    (row : int)
    (col : int)
    (board : Scrabble.board) = 
  name >:: (fun _ -> 
      assert_equal expected 
        (Scrabble.special_tostr
           (Scrabble.get_spot_from_board board row col).special)
        ~printer:(fun x -> x))


(******************Tests for Scrabble.ml************************************)

(* Boards for use in scrabble_test... *)
let b1 = Scrabble.make_empty_board 3 [] [] [] [] [(1,1)]
let b2 = 
  Scrabble.add_word [{l = 'D' ; p = 2}; {l = 'O' ; p = 1};] 
    "down" (1 , 1) b1 true
let b3 = begin 
  Scrabble.add_word [{l = '_' ; p = 0}; {l = 'D' ; p = 2}; {l = 'E' ; p = 1}]
    "across" (2 , 1) b2 false
end

let peace = [{l = 'P' ; p = 2}; {l = 'E' ; p = 1}; {l = 'A' ; p = 1}; 
             {l = 'C' ; p = 4}; {l = 'E' ; p = 1};]
let love = [{l = 'L' ; p = 2}; {l = 'O' ; p = 1}; {l = 'V' ; p = 5}; 
            {l = '_' ; p = 1};]
let thirty = [{l = 'T' ; p = 1}; {l = 'H' ; p = 2}; {l = 'I' ; p = 1}; 
              {l = 'R' ; p = 1}; {l = 'T' ; p = 1} ; {l = 'Y' ; p = 2}]
let one = [{l = '_' ; p = 1}; {l = 'N' ; p = 2}; {l = 'E' ; p = 1}]
let ten  = [{l = '_' ; p = 1}; {l = '_' ; p = 1}; {l = 'N' ; p = 2}]
let tea = [{l = 'T' ; p = 1}; {l = 'E' ; p = 1}; {l = 'A' ; p = 1}]
let it =  [{l = 'I' ; p = 1}; {l = 'T' ; p = 1}]
let et =  [{l = 'E' ; p = 1}; {l = 'T' ; p = 1}]

(* Add “peace, love, thirty, one, ten to empty 11 x 11 board *) 
let e1 = Scrabble.make_empty_board 11 [] [] [] [] [(5,2)] 
let e2 = Scrabble.add_word peace "across" (5,2) e1 true 
let e3 =  Scrabble.add_word love "down" (2,3) e2 false
let e4 = Scrabble.add_word thirty "across" (4,6) e3 false 
let e5 = Scrabble.add_word one "across" (3,3) e4 false 
let e6 = Scrabble.add_word ten "down" (4,6) e5 false
let e7 = Scrabble.add_word tea "across" (3,8) e6 false

let b_special = Scrabble.make_empty_board 7 
    [(2,1);(3,5)] [(1,4)] [(1,1);(7,7)] [(1,7);(7,1);(7,6)] [(4,4)]
let b_special_1 = Scrabble.add_word thirty "down" (2,4) b_special true
let b_special_2 = Scrabble.add_word thirty "down" (1,4) b_special true 
let b_special_3 = Scrabble.add_word it "down" (2,5) b_special_2 false
let b_special_4 = Scrabble.add_word et "across" (7,5) b_special_1 false 

(* Testing... *)
let scrabble_tests = [
  fails_well "Adding word not on start tile raises InvalidMove" (InvalidMove)
    (fun () -> Scrabble.add_word it "down" (1,1) e1 true);
  special_test "(1,1) on e1 is not special" "None" 1 1 e1; 
  special_test "(2,1) on b_special is bouble letter" "Double letter"
    2 1 b_special;
  special_test "(4,4) on b_special is start" "Start tile" 4 4 b_special;
  check_tile_at_spot "D at (1,1)" b2 1 1 (Some {l = 'D' ; p = 2});
  check_tile_at_spot "O at (2,1)" b2 2 1 (Some {l = 'O' ; p = 1});
  check_tile_at_spot "(3,1) is empty" b2 3 1 None;
  check_tile_at_spot "O at (2,1)" b3 2 1 (Some {l = 'O' ; p = 1});
  check_tile_at_spot "D at (2,2)" b3 2 2 (Some {l = 'D' ; p = 2});
  check_tile_at_spot "E at (2,3)" b3 2 3 (Some {l = 'E' ; p = 1}); 
  fails_well "Trying to add up raises UnknownDirection" 
    (UnknownDirection "up") 
    (fun () -> Scrabble.add_word [{l = 'G'; p = 2}] "up" (3 , 3) b3 false);
  fails_well "Trying to add to coordinate (4,4) raises BadCoordinates" 
    (BadCoordinates (4 , 4)) 
    (fun () -> Scrabble.add_word [{l = 'G'; p = 2}] "down" (4 , 4) b3 false);


  (*tests for valid_move*)
  make_valid_move_test "creating b3 is valid" 
    [{l = '_' ; p = 0}; {l = 'D' ; p = 2}; {l = 'E' ; p = 1}] "across" (2 , 1)
    b2 false; 
  fails_well "valid_move catches unknown directions" (UnknownDirection "wrong") 
    (fun () -> Scrabble.valid_move [{l = 'A'; p = 1}] "wrong" (1 , 1) b1 true);
  fails_well "invalid to put a '_' on a None tile, first spot" InvalidMove
    (fun () -> Scrabble.valid_move [{l = '_'; p = 0}] "down" (1 , 1) b1 true);
  fails_well "invalid to put a letter on Some tile, first spot" InvalidMove
    (fun () -> Scrabble.valid_move [{l = 'A'; p = 1}] "down" (2 , 1) b2 false);
  fails_well "invalid to put a '_' on a None tile, second spot" InvalidMove
    (fun () -> Scrabble.valid_move [{l = '_'; p = 0}; {l = '_'; p = 0}]
        "down" (2 , 3) b3 false);
  fails_well "invalid to put a letter on Some tile, second spot" InvalidMove
    (fun () -> Scrabble.valid_move [{l = 'A'; p = 1}; {l = 'E'; p = 1}]
        "down" (1 , 3) b3 false);
  make_valid_move_test "valid addition of one letter" [{l = 'A'; p = 1}] "down"
    (1 , 2) b3 false;
  make_valid_move_test "valid addition of two letters, second blank" 
    [{l = 'A'; p = 1}; {l = '_'; p = 0}] "down" (1 , 2) b3 false;
  make_valid_move_test "valid addition of two letters, first blank" 
    [{l = '_'; p = 0}; {l = 'A'; p = 1}] "across" (1 , 1) b3 false;
  fails_well "word is disconnected from rest of board" InvalidMove 
    (fun () -> Scrabble.valid_move [{l = 'I'; p = 1}; {l = 'S'; p = 1}]
        "down" (1 , 3) b2 false); 
  fails_well "invalid coordinates test for valid_move" (BadCoordinates (3 , 4))
    (fun () -> Scrabble.valid_move [{l = 'Z'; p = 10}; {l = 'Q'; p = 10}] 
        "across" (3 , 3) b2 false); 
  fails_well "Attempting a play that creates a non-word" (InvalidWord "HS")
    (fun () -> Scrabble.add_word [{l = '_'; p = 0}; {l = '_'; p = 0}; 
                                  {l = '_'; p = 0}; {l = '_'; p = 0}; 
                                  {l = '_'; p = 0}; {l = 'S'; p = 1}] 
        "across" (5,2) e7 false);
  make_valid_move_test "testing to see if first word can be alone" 
    [{l = 'A'; p = 1}; {l = 'T'; p = 1}] "down" (1 , 1) b1 true;

  score_test "DO on b2 should have score 6" 
    [{l = 'D' ; p = 2}; {l = 'O' ; p = 1};] "down" (1, 1) b2 3 6;
  score_test "ODE on b3 should have score 4" 
    [{l = '_' ; p = 0}; {l = 'D' ; p = 2}; {l = 'E' ; p = 1}] 
    "across" (2, 1) b3 3 4;
  score_test "TEA on b7 has score 9" 
    [{l = 'T' ; p = 1}; {l = 'E' ; p = 1}; {l = 'A' ; p = 1}] "across" 
    (3,8) e7 11 9;
  score_test "thirthy on b_special_1 has score 16" thirty "down" (2,4) 
    b_special_1 7 16;
  score_test "thirthy on b_special_2 has score 20" thirty "down" (1,4) 
    b_special_2 7 20;
  score_test "it on b_special_3 has score 9, checks multi words" it "down" 
    (2,5) b_special_3 7 9;
  score_test "et with preexisting starting letter on b_special_4" it "across"
    (7,5) b_special_4 7 12;

  fails_well "Adding ACE on e4 raises InvalidWord" (InvalidWord "ATHIRTY")
    (fun () -> Scrabble.add_word 
        [{l = 'A' ; p = 1}; {l = '_' ; p = 1}; {l = 'E' ; p = 1}] 
        "down" (4,5) e4 false);

]

(**************************************************************************)
(**************************gameState.ml testing****************************)

let from_json_test
    (name : string)
    (json : Yojson.Basic.t)
    (expected : GameState.t) =
  name >:: (fun _ -> 
      assert_equal expected (GameState.from_json json)) 

let num_letters_left_test 
    (name : string) 
    (state : GameState.t)
    (expected : int) = 
  name >:: (fun _ -> 
      assert_equal expected (GameState.num_letters_left state) 
        ~printer:string_of_int)

let get_letter_test 
    (name : string) 
    (state : GameState.t)
    (expected : Scrabble.tile option) = 
  name >:: (fun _ -> 
      assert_equal expected (GameState.get_letter state) 
        ~printer:print_toption)

let remove_front_tile_test 
    (name : string) 
    (state : GameState.t)
    (expected : GameState.t) = 
  name >:: (fun _ -> 
      assert_equal expected (GameState.remove_front_tile state))

let update_score_test
    (name : string)
    (pname : string)
    (state : GameState.t)
    (score : int)
    (expected : int) = 
  let new_s = (GameState.update_score state pname score) in 
  name >:: (fun _ -> 
      assert_equal expected (get_player new_s pname).score 
        ~printer:string_of_int)

let assign_test
    (name : string)
    (state : GameState.t)
    (pname : string)
    (tlst : Scrabble.tile list)  
    (expected : Scrabble.tile list) = 
  name >:: (fun _ -> 
      assert_equal expected (GameState.assign_pts state pname tlst) 
        ~printer:print_tlst)

let whose_turn_test 
    (name : string)
    (state : GameState.t)
    (expected : string) = 
  name >:: (fun _ -> assert_equal expected ((GameState.whose_turn state).name)
               ~printer:(fun x -> x))

let check_score
    (name : string)
    (expected : int)
    (state : GameState.t)
    (pname : string) = 
  name >:: (fun _ -> assert_equal expected 
               (GameState.get_player state pname).score ~printer:string_of_int)

(*necessary variables*)
let test_json = Yojson.Basic.from_file "test_game.json"
let test_state = GameState.from_json test_json
let bag_test = [{l = 'B'; p = 3}; {l = 'A'; p = 1}; {l = 'A'; p = 1}]
let game_state_test0 : GameState.t = {bag = bag_test; 
                                      board = b1; 
                                      players = [];
                                      turn_number = 0;}
let tileB = get_letter test_state
let game_state_test1 = remove_front_tile test_state
let game_state_test2 = remove_front_tile game_state_test1
let game_state_test3 = remove_front_tile game_state_test2
let player1 = {
  name = "Sophie"; 
  score = 0; 
  tile_rack = [{l = 'A'; p = 1}; {l = 'M'; p = 1}; {l = 'Z'; p = 5}];
}
let player2 = {
  name = "Gavin"; score = 12; tile_rack = [{l = 'P'; p = 1}; {l = 'O'; p = 1}; 
                                           {l = 'P'; p = 1}];
}

let standard_json = Yojson.Basic.from_file "standard_game.json"
let standard_state0 = GameState.from_json standard_json
let shuffled_state = GameState.shuffle_bag standard_state0
let with_players = GameState.add_player game_state_test0 player1 
                   |> (fun x -> GameState.add_player x player2)
let st2 = GameState.pass_turn with_players
let st3 = GameState.go_to_new_state with_players 
    ([{l = 'P'; p = 1}; {l = 'O'; p = 1}; {l = 'P'; p = 1}]) (1,1) "down" 3


(*testing*)
let gameState_tests = [

  num_letters_left_test "using test of length 3" test_state 3; 
  num_letters_left_test "removed one letter" game_state_test1 2;
  num_letters_left_test "standard game" standard_state0 100; 
  num_letters_left_test "empty bag" game_state_test3 0;
  get_letter_test "get B tile" game_state_test0 (Some {l = 'B'; p = 3});
  get_letter_test "returns None" game_state_test3 None;
  update_score_test "Add score to Sophie" "Sophie" with_players 10 10;
  update_score_test "Add score to Gavin" "Sophie" with_players 5 5;  
  assign_test "Proper pts assigned" with_players "Sophie" 
    [{l = 'A'; p = 10}; {l = 'M'; p = 10}; {l = 'Z'; p = 10}]
    [{l = 'A'; p = 1}; {l = 'M'; p = 1}; {l = 'Z'; p = 5}];
  fails_well "Trying to swap tiles not in rack raises exception" 
    (TilesNotinRack) (fun () -> GameState.swap_tiles with_players 
                         [{l = 'F'; p = 1}]);
  whose_turn_test "In st2, it's Sophie's turn" st2 "Sophie";
  special_test "Playing on special tile removes its special properties" 
    "None" 1 1 (st3.board);
  check_score "Gavin's score is 6 in st3" 18 st3 "Gavin";
  whose_turn_test "In st3, it's Sophie's turn" st3 "Sophie";


]

(*****************************************************************)
(***********************command tests*****************************)

(*helper functions*)
let parser_test
    (name : string)
    (str : string)
    (expected : Command.command) : test = 
  name >:: (fun _ -> assert_equal expected (Command.parser str))

(* variables *)
let parser0 : Command.play_move = {
  tiles = [{l = 'T'; p = 1}; {l = 'I'; p = 1}];
  location = (1, 1); 
  direction = "down" }
let parser1 : Command.play_move = {
  tiles = [{l = 'A'; p = 1}; {l = 'I'; p = 1}];
  location = (5, 11); 
  direction = "across" }


(*testing*)
let command_tests = [
  parser_test "quit test, no changing case" "QUIT" Quit;
  parser_test "quit test, handles case" "Quit" Quit;
  parser_test "play hi, first loc" "PLAY TI (1,1) down" (Play parser0);
  parser_test "play hi, first loc" "PLAY AI (5,11) across" (Play parser1);
  parser_test "testing help function" "help" Help;
  parser_test "testing multi word handling" "exit" Quit;
  parser_test "testing pass" "PASS" Pass;
  parser_test "test swap" "Swap AI" (Swap (parser1.tiles));


  fails_well "invalid command" EmptyCommand 
    (fun () -> Command.parser "");
  fails_well "invalid command" MalformedCommand 
    (fun () -> Command.parser "you cant do this!!!");
  fails_well "too many spaces" MalformedCommand 
    (fun () -> Command.parser "play hi (1 , 1) down");
  fails_well "wrong location" MalformedCommand
    (fun () -> Command.parser "play hi (1 , A) down");
  fails_well "not a location" MalformedCommand
    (fun () -> Command.parser "play hi (Apple!) down");

]


(***********************testing suite****************************)
let tests =
  "test suite for Project"  >::: List.flatten [
    scrabble_tests;
    gameState_tests;
    command_tests;
  ]

let _ = run_test_tt_main tests