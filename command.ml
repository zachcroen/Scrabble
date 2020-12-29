
exception MalformedCommand
exception EmptyCommand

type play_move = {tiles : Scrabble.tile list;
                  location : (int * int);
                  direction : string}

type command = 
  | Play of play_move
  | Swap of Scrabble.tile list 
  | Help 
  | Quit 
  | Pass 

(*filters out the empty string*)
let filter_func (str : string) : bool = 
  match str with 
  | "" -> false
  | _ -> true

let word_to_tile_lst word_str = 
  let char_lst str = List.init (String.length str) (String.get str) in
  let rec tile_lst_maker_0 (acc : Scrabble.tile list) (chars : char list) 
    : Scrabble.tile list = 
    match chars with 
    | [] -> acc
    | h :: t -> begin 
        let new_tile : Scrabble.tile = 
          if Char.lowercase_ascii h = h then
            {l = Char.uppercase_ascii h; p = 0} else {l = h; p = 1} in 
        let new_acc = new_tile :: acc
        in tile_lst_maker_0 new_acc t 
      end
  in char_lst word_str |> List.rev |> tile_lst_maker_0 []

let get_location loc_str = 
  let split_on_comma str =
    String.sub str 1 (String.length str - 2) 
    |> String.split_on_char ',' 
    |> List.filter filter_func in
  match split_on_comma loc_str with 
  | first :: last :: empty when empty = [] -> begin
      try ((int_of_string first), (int_of_string last)) with
      | _ -> raise MalformedCommand
    end
  | _ -> raise MalformedCommand

let capitalize_firstword word_lst = 
  match word_lst with 
  | [] -> word_lst 
  | h :: t -> begin 
      let first_caps = String.uppercase_ascii h in 
      first_caps :: t
    end 

let parser s = 
  let spliced = s 
                |> String.split_on_char ' '
                |> List.filter filter_func 
                |> capitalize_firstword
  in match spliced with 
  | [] -> raise EmptyCommand
  | h :: t when (h = "QUIT" || h = "EXIT") && t = [] -> Quit
  | h :: t when (h = "PASS" || h = "SKIP") && t = [] -> Pass
  | h :: t when (h = "HELP" || h = "INSTRUCTIONS" ) && t = [] -> Help
  | h :: (m :: t) when (h = "SWAP") && t = [] -> begin 
      let tile_lst = word_to_tile_lst m in 
      Swap tile_lst
    end
  | h :: word :: loc :: direc :: fin when 
      h = "PLAY" && fin = [] -> begin
      let tile_lst = word_to_tile_lst word in 
      let location = get_location loc in
      let this_move : play_move = {
        tiles = tile_lst;
        location = location;
        direction = String.lowercase_ascii direc} in 
      Play this_move
    end
  | _ -> raise MalformedCommand
