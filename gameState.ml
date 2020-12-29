(*Use Yojson.Basic.Util to parse JSON files *)
open Yojson.Basic.Util

exception EmptyBag
exception BadJson
exception TilesNotinRack


type plyr = {
  name : string;
  score : int;
  tile_rack : Scrabble.tile list;
}

type t = {
  bag : Scrabble.tile list;
  board : Scrabble.board;
  players : plyr list;
  turn_number : int;
}

(* intermediate type for parsing json *)
type game_tiles = {
  letter : string;
  points : int;
  count : int
}

(*helper for shuffle bag *)
let random_compare item_1 item_2 = 
  let rand1 = Random.int 10 in
  let rand2 = Random.int 10 in
  if rand1 > rand2 then -1 else 1

let shuffle_bag state = 
  let shuffled = List.sort random_compare state.bag in 
  {
    bag = shuffled;
    board = state.board;
    players = state.players;
    turn_number = state.turn_number;
  }

let get_start s = Scrabble.get_start s.board 

let num_letters_left state = List.length (state.bag)

let get_letter state = match state.bag with 
  | [] -> None 
  | h :: t -> Some h 

(*********************** From json ***********************)

(* helper to convert a string of form "(int1,int2)" to (int1,int2), can be used
   with List.map as a function. 
   Precondition: input string must be of specified format *)
let inttup_of_string str = 
  let str_length = String.length str in 
  let sub_str = String.sub str 1 (str_length - 2) in 
  let str_lst = String.split_on_char ',' sub_str in
  match str_lst with 
  | h :: (m :: t) when t = [] -> int_of_string h, int_of_string m
  | _ -> raise BadJson

let game_tiles_params json = {
  letter = json 
           |> member "letter" 
           |> to_string;
  points = json 
           |> member "points" 
           |> to_int;
  count = json
          |> member "count" 
          |> to_int
} 

let get_special_tiles str json = 
  json
  |> member "special_tiles"
  |> member str 
  |> to_list
  |> List.map to_string
  |> List.map inttup_of_string

let game_tiles json = json 
                      |> member "game_tiles" 
                      |> to_list 
                      |> List.map game_tiles_params 

let tiles game_tiles : Scrabble.tile list = 
  let rec tiles_helper remain_tiles acc = 
    match remain_tiles with
    | [] -> acc
    | h :: t -> begin 
        match h.letter, h.points, h.count with 
        | a, b, c when c = 1 -> begin
            let new_tile : Scrabble.tile = {l = a.[0]; p = b} in 
            let new_acc = new_tile :: acc in 
            tiles_helper t new_acc
          end
        | a, b, c -> begin
            let new_tile : Scrabble.tile = {l = a.[0]; p = b} in
            let new_acc = new_tile :: acc in 
            let new_head = {letter = a; points = b; count = c-1} in 
            tiles_helper (new_head :: t) new_acc
          end
      end 
  in tiles_helper game_tiles [] 

let from_json json = 
  (* parse all elements from json *)
  let size = json 
             |> member "board_size" 
             |> to_int 
  in 
  let dbl_l = get_special_tiles "double_letter" json in 
  let trp_l = get_special_tiles "triple_letter" json in 
  let dbl_w = get_special_tiles "double_word" json in 
  let trp_w = get_special_tiles "triple_word" json in  
  let st_loc = get_special_tiles "starting_loc" json in   
  (*return value of from_json*)
  {bag = (tiles (game_tiles json));
   board = Scrabble.make_empty_board size dbl_l trp_l dbl_w trp_w st_loc;
   players = [];
   turn_number = 0}
  |> shuffle_bag

(****************** end from json ******************)

let remove_front_tile state = 
  match state.bag with 
  | h :: t -> 
    {bag = t; 
     board = state.board; 
     players = state.players;
     turn_number = state.turn_number 
    }
  | _ -> raise EmptyBag

let add_player state player =
  {bag = state.bag;
   board = state.board;
   players = player :: state.players;
   turn_number = state.turn_number;
  }

let get_player state name = 
  let p_list = state.players in 
  let rec helper lst name = match lst with 
    | [] -> failwith "Player not found"
    | h :: t -> if h.name = name then h else helper t name 
  in 
  helper p_list name 

let insert_newp player_lst player_name update =
  let rec helper player_lst player_name update acc = 
    match player_lst with 
    | [] -> failwith "Player is not in this list"
    | h :: t -> 
      if h.name = player_name then 
        (List.rev acc) @ [update] @ t 
      else 
        helper t player_name update (h :: acc)
  in 
  helper player_lst player_name update []

let update_score state player_name to_add = 
  let p = get_player state player_name in 
  let (p_updated : plyr) = {
    name = p.name;
    score = p.score + to_add;
    tile_rack = p.tile_rack;
  }
  in
  {
    bag = state.bag;
    board = state.board;
    players = insert_newp state.players player_name p_updated;
    turn_number = state.turn_number;
  }

let rec update_pts (rack : Scrabble.tile list) (tile : Scrabble.tile) = 
  match rack with 
  | [] -> raise TilesNotinRack
  | h :: t when tile.l = '_' -> 0
  | h :: t when h.l = tile.l -> h.p 
  | h :: t -> update_pts t tile

(*gets tile list, state, player, changes tlst to have right point values*)
let assign_pts state player (tlst : Scrabble.tile list) 
  : Scrabble.tile list =
  let start_rack = (get_player state player).tile_rack in 
  let rec give_points_helper rack acc (tlst : Scrabble.tile list) =  
    match tlst with
    | [] -> acc 
    | h :: t -> begin
        if h.p = 0 then give_points_helper rack (h :: acc) t
        else 
          let correct_points = update_pts rack h in 
          let new_tile : Scrabble.tile = {l = h.l; p = correct_points} in 
          give_points_helper rack (new_tile :: acc) t
      end
  in List.rev tlst |> give_points_helper start_rack []

let filt_prime (t : Scrabble.tile) = 
  if t.l = '_' then false else true 

let rec find_tile_and_remove 
    (tile : Scrabble.tile) 
    (rack : Scrabble.tile list) 
    (acc : Scrabble.tile list) = 
  if tile.p = 0 then ftr_aux rack tile acc 
  else ftr_else rack tile acc 

and ftr_else rack tile acc =
  match rack with 
  | [] -> raise TilesNotinRack
  | h :: t -> 
    if h = tile then acc @ t 
    else find_tile_and_remove tile t (h :: acc)

and ftr_aux rack tile acc = 
  match rack with 
  | [] -> raise TilesNotinRack
  | h :: t -> 
    if h.l = '/' then acc @ t
    else find_tile_and_remove tile t (h :: acc)

let chck_rck state player_name tlst =
  let start_rack = (get_player state player_name).tile_rack in 
  let filtered_lst = List.filter filt_prime tlst in 
  let rec helper (tile_lst : Scrabble.tile list) rack = 
    match tile_lst with 
    | [] -> state
    | h :: t -> 
      helper t (find_tile_and_remove h rack [])
  in 
  helper filtered_lst start_rack

let rm_from_rk curr_rack t = 
  let tlst = List.filter filt_prime t in 
  let rec find_tile_and_remove 
      (tile : Scrabble.tile) 
      (rack : Scrabble.tile list) 
      (acc : Scrabble.tile list) =
    if tile.p = 0 then 
      match rack with 
      | [] -> raise TilesNotinRack
      | h :: t when h.l = '/' -> acc @ t 
      | h :: t -> find_tile_and_remove tile t (h :: acc)
    else 
      match rack with 
      | [] -> raise TilesNotinRack
      | h :: t when h = tile -> acc @ t 
      | h :: t -> find_tile_and_remove tile t (h :: acc)
  in  
  let rec helper lister c_rack = match lister with 
    | [] -> c_rack
    | h :: t -> helper t (find_tile_and_remove h c_rack [])
  in 
  helper tlst curr_rack 

let draw_tiles state num_tiles = 
  let rec helper state cntr acc_tiles = 
    if cntr = 0 || state.bag = [] 
    then (state, acc_tiles)
    else match state.bag with 
      | h :: t -> 
        helper
          {
            bag = t; 
            board = state.board; 
            players = state.players;
            turn_number = state.turn_number
          } 
          (cntr - 1)
          (h :: acc_tiles)
      | _ -> (state, acc_tiles)
  in 
  helper state num_tiles []

let rm_from_prk p tlst = 
  {
    name = p.name;
    tile_rack = rm_from_rk p.tile_rack tlst;
    score = p.score;
  }

let whose_turn curr_state = List.nth curr_state.players 
    (curr_state.turn_number mod (List.length curr_state.players))

let filter_func (t_lst : Scrabble.tile) : bool = 
  match t_lst.l with 
  | '_' -> false
  | _ -> true

let inc_turn state = {
  bag = state.bag;
  board = state.board;
  players = state.players;
  turn_number = state.turn_number + 1
}

let rotate_plst state = 
  let new_player_lst = 
    match state.players with 
    | [] -> state.players  
    | h :: t -> h :: (List.rev t) |> List.rev 
  in {bag = state.bag;
      board = state.board;
      players = new_player_lst;
      turn_number = state.turn_number}

let rck_update player t_lst state = 
  {
    bag = state.bag;
    board = state.board;
    players = insert_newp state.players player.name 
        (rm_from_prk player t_lst);
    turn_number = state.turn_number
  } 

let fill_players_rack player t_lst state = 
  let (s , t) = 
    if state.bag = [] then (state, []) else 
      draw_tiles state (List.length (List.filter filter_func t_lst)) 
  in 
  {bag = s.bag;
   board = s.board;
   players = (insert_newp s.players player.name 
                {
                  name = player.name;
                  tile_rack = t @ (player.tile_rack);
                  score = player.score
                });
   turn_number = s.turn_number}

let add_tobag tile_lst state = 
  {bag = tile_lst @ state.bag;
   board = state.board;
   players = state.players;
   turn_number = state.turn_number} 
  |> shuffle_bag

let pass_turn state = 
  if state.turn_number = 0 then rotate_plst state 
  else inc_turn state 

let rec print_end l = match l with 
  | [] -> print_string "~~~~~~~~~~~~~~~~~~\n"
  | h :: t -> 
    ANSITerminal.(print_string [green] (fst h));
    print_string (" : ");
    print_string (string_of_int (snd h) ^ "\n"); 
    print_end t

let rec get_final_scores plst acc = match plst with
  | [] -> acc
  | h :: t -> 
    let subtract_me = if h.tile_rack = [] then 0 
      else 
        let f = (fun sum (x : Scrabble.tile) -> x.p + sum) in 
        List.fold_left f 0 h.tile_rack 
    in 
    let new_score = h.score - subtract_me in 
    get_final_scores t ((h.name, new_score) :: acc)


let end_game st =
  let plst = st.players in 
  let printme = get_final_scores plst [] in 
  print_string ("\n\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n");
  Scrabble.board_printer2 st.board;
  ANSITerminal.(print_string [magenta] "\nFINAL SCORES:\n\n");
  print_end printme;
  {
    bag = st.bag;
    board = st.board;
    players = st.players;
    turn_number = -1;
  }


let adder t_lst direction start_coor x = 
  { 
    bag = x.bag;
    board = Scrabble.add_word t_lst direction start_coor 
        x.board (x.turn_number = 0);
    players = x.players;
    turn_number = x.turn_number; 
  }

let update_special x = 
  {
    bag = x.bag;
    board = Scrabble.elim_specials x.board;
    players = x.players;
    turn_number = x.turn_number;
  }

let fix_prack my_turn t_lst x = 
  {
    bag = x.bag;
    board = x.board;
    players = insert_newp x.players my_turn.name 
        (rm_from_prk (get_player x my_turn.name) 
           t_lst);
    turn_number = x.turn_number
  }

let newst_aux my_turn t_lst x =
  let (s , t) = draw_tiles x (List.length (List.filter filter_func t_lst)) 
  in 
  { 
    bag = s.bag;
    board = s.board;
    players = (insert_newp s.players my_turn.name 
                 {
                   name = my_turn.name;
                   tile_rack = t @ 
                               ((get_player s my_turn.name).tile_rack);
                   score = ((get_player s my_turn.name).score)
                 });
    turn_number = s.turn_number; 
  }

let go_to_new_state curr_state t_lst start_coor direction board_size = 
  let my_turn = whose_turn curr_state in 
  chck_rck curr_state my_turn.name t_lst 
  |> adder t_lst direction start_coor
  |> (fun x -> update_score x my_turn.name 
         (Scrabble.score_word t_lst direction start_coor x.board board_size))
  |> update_special
  |> fix_prack my_turn t_lst
  |> newst_aux my_turn t_lst 
  |> (fun s -> 
      let my_turn = whose_turn s in 
      if my_turn.tile_rack = [] then end_game s
      else inc_turn s)

let swap_aux correct_lst player x = 
  if x.bag = [] then x else begin 
    let (s , t) = draw_tiles x 
        (List.length (List.filter filter_func correct_lst)) 
    in 
    let new_rec = 
      insert_newp s.players player.name 
        {
          name = player.name;
          tile_rack = t @ 
                      ((get_player s player.name).tile_rack);
          score = ((get_player s player.name).score)
        }
    in 
    {
      bag = s.bag;
      board = s.board;
      players = new_rec;
      turn_number = s.turn_number;
    } end

let swap_tiles state tile_lst =  
  if state.bag = [] then raise EmptyBag else 
    let player = whose_turn state in 
    let correct_lst = assign_pts state player.name tile_lst in
    add_tobag correct_lst state  
    |> (fun x -> {
          bag = x.bag;
          board = x.board;
          players = insert_newp x.players player.name 
              (rm_from_prk player correct_lst);
          turn_number = x.turn_number
        })
    |> swap_aux correct_lst player
    |> pass_turn





