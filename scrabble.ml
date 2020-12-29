open Csv 

type letter = char 
type points = int 

type tile = {
  l : letter; 
  p : points
}

(*represent special locations on a scrabble board*)
type specials = DoubleLetter
              | TripleLetter
              | DoubleWord
              | TripleWord
              | StartTile 
              | None

type spot_specs  = {
  tile : tile option;
  special : specials;
} 

(* First element of tuple is the row. First element of the second tuple 
   is the column. *)
type spot = int * (int * spot_specs)

type board = spot list

(*Possible game state Exceptions *)
exception UnknownDirection of string
exception BadCoordinates of int * int 
exception InvalidMove
exception InvalidWord of string 

let special_tostr (s : specials) = match s with 
  | DoubleLetter -> "Double letter"
  | TripleLetter -> "Triple letter"
  | DoubleWord -> "Double word"
  | TripleWord -> "Triple word"
  | StartTile -> "Start tile"
  | None -> "None" 

let board_length board =
  int_of_float (sqrt (float_of_int (List.length board)))

let rec get_start board = 
  match board with 
  | [] -> raise Not_found
  | h :: t -> 
    match (snd (snd h)).special with 
    | StartTile -> (fst h, (fst (snd h)))
    | _ -> get_start t   

let get_spot_from_board board row col : spot_specs = 
  List.filter_map (fun x -> if (fst x) = row then Some x else None) board 
  |> List.split 
  |> snd 
  |> List.find (fun x -> fst x = col) 
  |> snd 

let get_tile_from_board (board : board) (row : int) (col : int) = 
  let spot = get_spot_from_board board row col in 
  spot.tile

(*Returns true if any adjacent tile has Some tile, otherwise false *)
let check_tile_is_some_safe (board : board) (col : int) (row: int) : bool =
  try match get_tile_from_board board col row with 
    | None -> false
    | Some tile -> true 
  with
  | _ -> false 

(*[check_adjacents board coordinate] returns the true if an adjacent tile
  to [coordinate] on [board] is Some tile*)
let check_adjacents (board : board) (coordinate : int * int) : bool =
  let row = fst coordinate in
  let col = snd coordinate in 
  let left = check_tile_is_some_safe board row (col - 1) in
  let right = check_tile_is_some_safe board row (col + 1) in
  let above = check_tile_is_some_safe board (row - 1) col in
  let below = check_tile_is_some_safe board (row + 1) col in
  above || below || left || right 

(*[check_tile_at_spot] is a [valid_move] helper that checks to make sure that 
  if the tile is '_' then a letter is already in that spot on the board, 
  and if the tile is not '_', the board has a tile of None *)   
let check_tile_at_spot tile coordinate board adjacent : bool =
  let tile_at_loc = 
    try get_tile_from_board board (fst coordinate) (snd coordinate) with 
    | _ -> raise (BadCoordinates ((fst coordinate) , (snd coordinate))) 
  in match tile.l with
  | '_' -> begin 
      if tile_at_loc = None then raise InvalidMove
      else adjacent || (check_adjacents board coordinate) 
    end
  | _ -> begin 
      if tile_at_loc = None then adjacent || (check_adjacents board coordinate) 
      else raise InvalidMove
    end  


(*helper for checking valid moves, steps in direction of word*)
let next_coord start_coor direction = 
  match direction with 
  | "across" -> (fst start_coor, (snd start_coor) + 1)
  | "down" -> ((fst start_coor) + 1, snd start_coor)
  | str -> raise (UnknownDirection str)

(*helper for checking first move by checking individual spot*)
let check_for_starttile start_coor board = 
  let spot = get_spot_from_board board (fst start_coor) (snd start_coor) in
  match spot.special with 
  | StartTile -> true 
  | _ -> false

(* helper for check valid move, checks if any tiles are on the start tile*)
let rec check_first_move word direction start_coor board = 
  match word with 
  | [] -> raise InvalidMove
  | h :: t -> begin 
      if check_for_starttile start_coor board 
      then () 
      else let next_coords = next_coord start_coor direction in 
        check_first_move t direction next_coords board  
    end

(* [valid_move] raises Invalid_Move if a word is played such that any
   tile is not adjacent to other tiles, or UnknownDirection if an invalid
   direction is given.
   Current implementation does this by checking if any tile is not connected
   to any other tile.  *)
let valid_move 
    (word : tile list) 
    (direction : string) 
    (start_coor : int * int) 
    (board : board)
    (first_move : bool) : unit =
  let rec valid_move_helper (word : tile list) (direction : string) 
      (start_coor : int * int) (board : board) (acc : bool) : bool =
    match word with 
    | [] -> acc
    | h :: t -> begin
        let next_coords = next_coord start_coor direction in
        let adjacent_bool = check_tile_at_spot h start_coor board acc in 
        valid_move_helper t direction next_coords board adjacent_bool
      end
  in let adjacent_tiles = 
       valid_move_helper word direction start_coor board false in
  match adjacent_tiles , first_move with
  | false, false -> raise InvalidMove
  | _, true -> check_first_move word direction start_coor board
  | _, _ -> ()

(*****************************************************************)

(* This is a helper for [make_empty_board] that looks up if a location
   should have a special attribute (ie double word etc) *)
let lookup_special (col : int) (row : int) dl tl dw tw st : specials = 
  let tuple = col , row in 
  let dbl_l = List.mem tuple dl in 
  let trp_l = List.mem tuple tl in 
  let dbl_w = List.mem tuple dw in 
  let trp_w = List.mem tuple tw in
  let st_loc = List.mem tuple st in 
  match st_loc, trp_w, dbl_w, trp_l, dbl_l with
  | true, _, _, _, _ -> StartTile 
  | _, true, _, _, _ -> TripleWord
  | _, _, true, _, _ -> DoubleWord 
  | _, _, _, true, _ -> TripleLetter
  | _, _, _, _, true -> DoubleLetter
  | _, _, _, _, _ -> None 


(* This is a helper for [make_empty_board]. *)
let rec add_row board n row dl tl dw tw st : board = 
  match n with 
  | i when i = 0 -> board
  | i -> begin 
      let special_spot = lookup_special row n dl tl dw tw st in  
      let s = {tile = None ; special = special_spot} in 
      add_row ((row , (n , s)) :: board) (n - 1) row dl tl dw tw st
    end

let make_empty_board n dl_lst tl_lst dw_lst tw_lst st_lst : board = 
  let rec helper board rcnt = 
    match rcnt with 
    | i when i = 0 -> board 
    | i -> begin
        let new_board = add_row board n i dl_lst tl_lst dw_lst tw_lst st_lst in  
        helper new_board (i - 1) 
      end
  in helper [] n 

(* This is a helper for [add_tile]. *)
let is_spot h row col = 
  if (fst h) <> row then false else 
  if (fst (snd h)) = col then true 
  else false

(* Helper for [add_word]. 
   [add_tile r c t b] is a new board that is identical to [b], excpet that
   tile [t] has been added to the spot in row [r] and column [c]. *)
let add_tile row col tile board = 
  let rec helper row col acc board = match board with 
    | [] -> raise (BadCoordinates (row , col))
    | h :: t -> 
      let s = (snd (snd h)).special in 
      let my_t = (row , (col , { tile = Some tile ; special = s})) in 
      if is_spot h row col then (List.rev acc) @ (my_t :: t)
      else helper row col (h :: acc) t
  in 
  helper row col [] board 

(* Helper for [add_word]. *)
let add_across tiles (start : int * int) board = 
  let row = (fst start) in 
  let rec helper tiles col board = match tiles with 
    | [] -> board 
    | h :: t -> 
      if h.l = '_' then helper t (col + 1) board
      else helper t (col + 1) (add_tile row col h board)
  in 
  helper tiles (snd start) board

(* Helper for [add_word]. *)
let add_down tiles (start : int * int) board = 
  let col = (snd start) in 
  let rec helper tiles row board = match tiles with 
    | [] -> board 
    | h :: t -> 
      if h.l = '_' then helper t (row + 1) board
      else helper t (row + 1) (add_tile row col h board)
  in 
  helper tiles (fst start) board

let add_word_no_dict 
    (tile_lst : tile list) 
    (direction : string) 
    (start_coor : int * int) 
    (board : board)
    (first_move : bool) : board = 
  if direction = "across" then add_across tile_lst start_coor board else
  if direction = "down" then add_down tile_lst start_coor board 
  else raise (UnknownDirection direction)

(*********************** Dictionary Checker ***********************)

(*returns a uppercase string from tile list *)
let tile_lst_to_string tile_lst = 
  let rec char_lst acc tile_lst = 
    match tile_lst with 
    | [] -> acc
    | h :: t -> begin
        let new_acc = h.l :: acc in 
        char_lst new_acc t 
      end 
  in let inorder_lst = List.rev tile_lst 
                       |> char_lst [] in 
  String.concat "" (List.map (String.make 1) inorder_lst) 
  |> String.uppercase_ascii

let dict_as_lst_aux arr =
  let rec helper a acc = match a with 
    | [] -> List.rev acc
    | h :: t -> helper t ((Array.get h 0) :: acc)
  in
  helper arr []

let dict_as_lst = 
  Csv.load "dict.csv" 
  |> Csv.to_array 
  |> Array.to_list 
  |> dict_as_lst_aux

let check_one_word_in_dict word dict = 
  if List.mem word dict then true 
  else raise (InvalidWord word)

let rec get_upper fst_coor acc board col = 
  let first_is_none = match (get_tile_from_board board fst_coor col) with 
    | None -> true 
    | Some _ -> false 
  in 
  if fst_coor = 0 || first_is_none then acc 
  else get_upper (fst_coor - 1) 
      ((get_tile_from_board board fst_coor col) :: acc) board col 


let down_aux1 word board start_coor = 
  let col = snd start_coor in  
  if fst start_coor <> 1 then 
    get_upper (fst start_coor - 1) [] board col 
  else 
    [] 

let rec get_lower fst_coor acc board col = 
  if fst_coor = board_length board + 1 then List.rev acc else
    let is_none = 
      match get_tile_from_board board fst_coor col with 
      | None -> true 
      | Some _ -> false 
    in 
    if is_none then List.rev acc 
    else get_lower (fst_coor + 1) 
        ((get_tile_from_board board fst_coor col) :: acc) board col 

let down_aux2 word board start_coor = 
  let col = snd start_coor in
  let beginning = down_aux1 word board start_coor in 
  beginning @ (get_lower (fst start_coor) []) board col
  |> List.map (fun x -> match x with 
      | Some t -> t 
      | None -> failwith "None found in listmap") 

let rec get_left board col acc row = 
  if col = 0 then acc else 
    let is_none = match get_tile_from_board board row col with  
      | None -> true 
      | Some _ -> false 
    in 
    if is_none then acc 
    else get_left board (col - 1) 
        ((get_tile_from_board board row col) :: acc) row 

let rec get_right board col acc row = 
  if col = (board_length board + 1) then List.rev acc else 
    let is_none = match get_tile_from_board board row col with  
      | None -> true 
      | Some _ -> false 
    in 
    if is_none then List.rev acc 
    else get_right board (col + 1) 
        ((get_tile_from_board board row col) :: acc) row 

let get_left_right board tile_coor =
  let row = fst tile_coor in 
  (get_left board (snd tile_coor) []) row 
  @ (get_right board (snd tile_coor + 1) []) row 

let rec check_all_lr board start_coor = 
  if fst start_coor = (board_length board + 1) then true 
  else do_rec board start_coor

and do_rec board start_coor = 
  match get_tile_from_board board (fst start_coor) (snd start_coor) with 
  | None -> true 
  | Some t -> 
    let f x = match x with 
      | Some t -> t 
      | None -> failwith "None in match" 
    in 
    let my_l = (tile_lst_to_string (List.map f
                                      (get_left_right board start_coor))) 
    in 
    if String.length my_l > 1 then 
      check_one_word_in_dict my_l dict_as_lst 
      && check_all_lr board ((fst start_coor + 1), (snd start_coor))
    else check_all_lr board ((fst start_coor + 1), (snd start_coor))

let do_down word board start_coor = 
  check_one_word_in_dict (tile_lst_to_string (down_aux2 word board start_coor))
    dict_as_lst && check_all_lr board start_coor

(** ACROSS! *)

let rec get_left fst_coor acc board row = 
  let first_is_none = match (get_tile_from_board board row fst_coor) with 
    | None -> true 
    | Some _ -> false 
  in 
  if fst_coor = 0 || first_is_none then acc 
  else get_left (fst_coor - 1) 
      ((get_tile_from_board board row fst_coor) :: acc) board row 


let aux1 word board start_coor = 
  let row = fst start_coor in  
  if snd start_coor <> 1 then 
    get_left (snd start_coor - 1) [] board row 
  else 
    [] 

let rec get_right fst_coor acc board row = 
  if fst_coor = board_length board + 1 then List.rev acc else
    let is_none = 
      match get_tile_from_board board row fst_coor with 
      | None -> true 
      | Some _ -> false 
    in 
    if is_none then List.rev acc 
    else get_right (fst_coor + 1) 
        ((get_tile_from_board board row fst_coor) :: acc) board row 


let aux2 word board start_coor = 
  let row = fst start_coor in
  let beginning = aux1 word board start_coor in  
  beginning @ (get_right (snd start_coor) []) board row 
  |> List.map (fun x -> match x with 
      | Some t -> t 
      | None -> failwith "None found in listmap") 


let rec get_up board row acc col = 
  if row = 0 then acc else 
    let is_none = match get_tile_from_board board row col with  
      | None -> true 
      | Some _ -> false 
    in 
    if is_none then acc 
    else get_up board (row - 1) 
        ((get_tile_from_board board row col) :: acc) col 


let rec get_down board row acc col = 
  if row = (board_length board + 1) then List.rev acc else 
    let is_none = match get_tile_from_board board row col with  
      | None -> true 
      | Some _ -> false 
    in 
    if is_none then List.rev acc 
    else get_down board (row + 1) 
        ((get_tile_from_board board row col) :: acc) col 


let get_up_down board tile_coor =
  let col = snd tile_coor in 
  get_up board (fst tile_coor) [] col 
  @ get_down board (fst tile_coor + 1) [] col 

let rec check_all_ud board start_coor = 
  if snd start_coor = (board_length board + 1) then true 
  else do_rec_case board start_coor

and do_rec_case board start_coor = 
  match get_tile_from_board board (fst start_coor) (snd start_coor) with 
  | None -> true 
  | Some t -> 
    let f x = match x with 
      | Some t -> t 
      | None -> failwith "None in match" in 
    let my_l = (tile_lst_to_string (List.map f
                                      (get_up_down board start_coor))) 
    in 
    if String.length my_l > 1 then 
      check_one_word_in_dict my_l dict_as_lst 
      && check_all_ud board ((fst start_coor), (snd start_coor + 1))
    else check_all_ud board ((fst start_coor), (snd start_coor + 1))

let do_across word board start_coor = 
  check_one_word_in_dict (tile_lst_to_string (aux2 word board start_coor))
    dict_as_lst && check_all_ud board start_coor

let word_in_dict word board start_coor direct : bool =
  let proxy_board = add_word_no_dict word direct start_coor board false in 
  if direct = "down" then do_down word proxy_board start_coor 
  else do_across word proxy_board start_coor

(******************************************************************)

let direct_cases tile_lst direction start_coor board = 
  if direction = "across" then add_across tile_lst start_coor board else
  if direction = "down" then add_down tile_lst start_coor board 
  else raise (UnknownDirection direction)

let add_word 
    (tile_lst : tile list) 
    (direction : string) 
    (start_coor : int * int) 
    (board : board)
    (first_move : bool) : board = 
  valid_move tile_lst direction start_coor board first_move; 
  if not (word_in_dict tile_lst board start_coor direction) 
  then raise (InvalidWord (tile_lst_to_string tile_lst))
  else direct_cases tile_lst direction start_coor board

let tile_opt_printer (tile : tile option) = match tile with 
  | None -> "None"
  | Some t -> "(" ^ (Char.escaped t.l) ^ " , " ^ (string_of_int t.p) ^ " pts)"

(*********************************************************)

(** [letter_from_spot x] is the char that is in spot [x] if there is a letter
    in spot x. else returns '_' *)
let letter_from_spot (x : spot) =
  let y = 
    x
    |> snd
    |> snd in
  match y.tile with
  | None -> '_'
  | Some z -> z.l 


(********************* printer function *************************)
let new_line n acc =
  if n = acc 
  then print_string ("\n"), 1
  else print_string (""), acc + 1

let rec printer2 (board : board) n acc =
  print_string (" ");
  match board with 
  | [] -> print_string ("\n")
  | h :: t -> print_help board n acc h t 

and print_help board n acc h t = 
  let specs = (snd (snd h)) in
  match specs.special with
  | DoubleLetter ->  
    ANSITerminal.(print_string [cyan] 
                    (Char.escaped (letter_from_spot h)));
    printer2 t n (snd (new_line n acc))
  | TripleLetter -> 
    ANSITerminal.(print_string [green] 
                    (Char.escaped (letter_from_spot h)));
    printer2 t n (snd (new_line n acc))
  | DoubleWord ->  
    ANSITerminal.(print_string [magenta] 
                    (Char.escaped (letter_from_spot h)));
    printer2 t n (snd (new_line n acc))
  | TripleWord ->  
    ANSITerminal.(print_string [red] 
                    (Char.escaped (letter_from_spot h)));
    printer2 t n (snd (new_line n acc)) 
  | StartTile ->  
    ANSITerminal.(print_string [yellow] 
                    (Char.escaped (letter_from_spot h)));
    printer2 t n (snd (new_line n acc))
  | None -> print_string 
              (Char.escaped (letter_from_spot h));
    printer2 t n (snd (new_line n acc))

let board_printer2 board =
  let n = board_length board in
  printer2 board n 1


(******************** Scoring a move ********************)

let find_special_effect spot = match spot.special with 
  | None -> (1 , 1)
  | DoubleLetter -> (2 , 1)
  | TripleLetter -> (3 , 1)
  | DoubleWord -> (1 , 2)
  | StartTile -> (1 , 2)
  | TripleWord -> (1 , 3)


let get_spot (board : board) (row : int) (col : int) = 
  List.filter_map (fun x -> if (fst x) = row then Some x else None) board 
  |> List.split 
  |> snd 
  |> List.find (fun x -> fst x = col) 
  |> snd 

let score_tlst tlst = 
  let rec helper lst acc = match lst with 
    | [] -> acc
    | h :: t -> helper t (acc + h.p)
  in 
  helper tlst 0

let look_up coors board = 
  let rec helper row col board acc =
    if row = 0 then acc 
    else match get_tile_from_board board row col with 
      | None -> acc
      | Some t -> helper (row - 1) col board (t :: acc)
  in 
  helper (fst coors - 1) (snd coors) board []

let look_down coors board = 
  let size = board_length board in 
  let rec helper row col board acc =
    if row = size + 1 then acc 
    else match get_tile_from_board board row col with 
      | None -> acc
      | Some t -> helper (row + 1) col board (t :: acc)
  in 
  helper (fst coors + 1) (snd coors) board []

let look_leftf row col board = 
  let rec helper row col board acc = 
    if col = 0 then acc
    else match get_tile_from_board board row col with 
      | None -> acc
      | Some t -> helper row (col - 1) board (t :: acc)  
  in 
  helper row col board []

let look_right_last_letter row col board =
  let size = board_length board in 
  let rec helper r c acc = 
    if c = size + 1 then acc
    else match get_tile_from_board board r c with
      | None -> acc
      | Some t -> helper r (c + 1) (t :: acc)
  in 
  helper row col []

let rec helper lst coor board acc = match lst with 
  | [] -> acc (* Scored all tiles *)
  | h :: t -> begin 
      let tf = h.l = '_' in 
      if tf then helper t (fst coor, snd coor + 1) board acc else 
        let spot = get_spot board (fst coor) (snd coor) in 
        let multipliers = find_special_effect spot in  
        let new_head = 
          match spot.tile with 
          | Some t -> t
          | None -> failwith "InvalidMove found in scoring 1"
        in
        let adj_lst = 
          (look_up coor board) @ (look_down coor board) 
        in 
        if (List.length adj_lst) = 0 then 
          helper t (fst coor, snd coor + 1) board acc 
        else 
          helper t (fst coor, snd coor + 1) board  
            (acc + ((snd multipliers) * (
                 ((fst multipliers) * new_head.p) + 
                 (score_tlst adj_lst)))) 
    end 


let score_across tlst coors board  = 
  let rec score_just_word tlst row col acc multiplier_acc = match tlst with 
    | [] -> (acc, multiplier_acc)
    | h :: t -> 
      let tile = match get_tile_from_board board row col with 
        | Some t -> t
        | None -> failwith "None found in score_just_word"
      in
      let s = find_special_effect (get_spot board row col) in 
      let new_score = acc + ((fst s) * tile.p) in 
      score_just_word t (row) (col + 1) new_score (multiplier_acc * (snd s))
  in
  let main_word = score_just_word tlst (fst coors) (snd coors) 0 1 in 
  helper tlst coors board 0  + 
  ((snd main_word) * 
   (score_tlst (look_leftf (fst coors) (snd coors - 1) board) +
    score_tlst (look_right_last_letter 
                  (fst coors) (snd coors + (List.length tlst)) board)
    + (fst main_word)))

let look_left coors board = 
  let rec helper row col board acc =
    if col = 0 then acc 
    else match get_tile_from_board board row col with 
      | None -> acc
      | Some t -> helper row (col - 1) board (t :: acc)
  in 
  helper (fst coors) (snd coors - 1) board []

let look_right coors board = 
  let size = board_length board in 
  let rec helper row col board acc =
    if col = size + 1 then acc 
    else match get_tile_from_board board row col with 
      | None -> acc
      | Some t -> helper row (col + 1) board (t :: acc)
  in 
  helper (fst coors) (snd coors + 1) board []

let look_upf row col board = 
  let rec helper row col board acc = 
    if row = 0 then acc else 
      match get_tile_from_board board row col with 
      | None -> acc
      | Some t -> helper (row - 1) col board (t :: acc)
  in 
  helper row col board []

let look_downl row col board = 
  let size = board_length board in 
  let rec helper row col board acc = 
    if row = size + 1 then acc 
    else match get_tile_from_board board row col with 
      | None -> acc
      | Some t -> helper (row + 1) col board (t :: acc)
  in 
  helper row col board []

let newh_aux spot = match spot.tile with 
  | Some t -> t
  | None -> failwith "InvalidMove found in scoring 2"

let rec down_help lst coor board acc = match lst with 
  | [] -> acc (* Scored all tiles *)
  | h :: t -> down_help_rec lst coor board acc h t 

and down_help_rec lst coor board acc h t = 
  let tf = h.l = '_' in 
  if tf then down_help t (fst coor + 1, snd coor) board acc else 
    let spot = get_spot board (fst coor) (snd coor) in 
    let multipliers = find_special_effect spot in  
    let new_head = newh_aux spot in
    let adj_lst = 
      (look_left coor board) @ 
      (look_right coor board) 
    in 
    if (List.length adj_lst) = 0 then 
      down_help t (fst coor + 1, snd coor) board acc 
    else 
      down_help t (fst coor + 1, snd coor) board  
        (acc + ((snd multipliers) * 
                ((fst multipliers) * (score_tlst [new_head])) + 
                (score_tlst adj_lst)))


let score_down tlst coors board  =  
  let rec score_just_word tlst row col acc multiplier_acc = 
    match tlst with 
    | [] -> (acc, multiplier_acc)
    | h :: t -> 
      let tile = 
        match get_tile_from_board board row col with 
        | Some t -> t
        | None -> failwith "None found in score_just_word" in
      let s = find_special_effect (get_spot board row col) in 
      let new_score = acc + ((fst s) * tile.p) in 
      score_just_word t (row + 1) col new_score (multiplier_acc * (snd s)) 
  in
  let main_word = score_just_word tlst (fst coors) (snd coors) 0 1 in 
  down_help tlst coors board 0  + 
  ((snd main_word) * 
   (score_tlst (look_upf (fst coors - 1) (snd coors) board) +
    score_tlst (look_downl 
                  (fst coors + (List.length tlst)) (snd coors) board)
    + (fst main_word)))

let score_word tlst direct coors board size = 
  if direct = "across" then score_across tlst coors board 
  else score_down tlst coors board 

(**************** End scoring ***************)

let rec elim_aux (b : board) (acc : board) = match b with 
  | [] -> List.rev acc 
  | h :: t -> elim_rec_case b acc h t 

and elim_rec_case b acc h t = 
  match (snd (snd h)).tile, (snd (snd h)).special with 
  | Some m, None -> elim_aux t (h :: acc)
  | Some m, _ ->
    let new_specs = {
      tile = (snd (snd h)).tile;
      special = None;
    }
    in elim_aux t ((fst h, (fst (snd h), new_specs)) :: acc)
  | _, _ -> elim_aux t (h :: acc)


let elim_specials board = elim_aux board []