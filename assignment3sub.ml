(* Programming Languages, Assignment 3 *)
(*
   You should write your functions in this file.
   You should specify the types of your functions, both for arguments and for
   return values.
   Write your code right below the corresponding comment describing the
   function you are asked to write.
*)

(* ----------------------------------------
            ROCK PAPER SCISSORS
   ---------------------------------------- *)

(*
   In the first set of exercises we will be describing in OCAML
   the game rock-paper-scissors. If somehow you are not familiar with the
   game, read it in wikipedia.
   We will specify some custom types for the game:
   - A "shape" is one of the possible hand shapes that a player can make.
   - A "check" is a pair of the two shapes that the two players are supposed
         to have made.
   - The "result" of a check is whether it was a tie, or whether the first
         or the second player won. For instance the result of (Rock, Paper)
         should be that the second player won.
   - A "game" is a list of "checks" that are supposed to happen in order.
   - A "valid game" is a game that could actually occur: Players only continue
         to perform "checks" if they run in a tie. For instance a valid game
         would be: [(Rock, Rock), (Rock, Paper)]
         But this is not valid: [(Rock, Rock), (Rock, Paper), (Paper, Scissors)]
         because the 3rd check should not be happening.
         Also this is not valid: [(Rock, Rock), (Paper, Paper)]
         because the game cannot end with ties.
         In other words a "valid game" would consist of a sequence of tied checks
         followed by one non-tied check, and nothing after it.
   - A game, even a non-valid one, can be "played" as follows:
      - Look at each check in the list in order. Return the first result that is
            not a Tie.
      - If all checks in the game are Ties, return a Tie.
   - A "play" is a list of shapes. They represent the intended "plays" of the player.
         For example [Rock, Rock, Paper] means that the player will play Rock on
         the first check, Rock on the second check and Paper on the third check.
*)
type shape = Rock | Paper | Scissors
type check = shape * shape
type result = Tie | FstWin | SndWin
type game = check list
type play = shape list

(*
   Write a function `result` that takes as input a check and
   returns the result of that check.
   Type: check -> result
*)

let result (r_check : check) : result =
   match r_check with
   | check -> 
      if fst check = snd check
      then Tie
      else if fst check > snd check
           then FstWin
           else SndWin


(*
   Write a function `is_tie` that takes as input a check and returns
   whether the check's result is a tie.
   Type: check -> bool
*)

let is_tie (t_check : check) : bool =
   match t_check with
   | check -> fst check = snd check

(*
   Write a function `game_from_plays` that takes as input two plays (correspoding
   to the intended plays of the two players) and creates a game by combining them,
   representing how they would have been played. If one play is longer than the
   other, stop at the shortest one.
   Type: play * play -> game
*)

let game_from_plays (play1, play2 : play * play) : game = 
   let rec aux (l1, l2) =
      match l1 with
      | [] -> []
      | h1 :: rest -> 
         match l2 with
         | [] -> []
         | h2 :: remaining -> (h1, h2) :: aux (rest, remaining)
   in aux (play1, play2)


(*
   Write a function `valid_game` that takes as input a game and determines if it is
   a valid game as described above.
   Type: game -> bool
*)

let rec valid_game (v_game : game) : bool = 
    match v_game with
    | [] -> false
    | check :: rest ->
        if is_tie (check) 
        then valid_game (rest)
        else if rest != []
             then false
             else true

(*
   Write a function `play_game` that plays the game as described above.
   Type: game -> result
*)

let rec play_game (p_game : game) : result = 
   match p_game with
   | check :: rest ->
      if is_tie check = true
      then play_game(rest)
      else result check

(* --------------------------------------
            TEMPERATURES
   -------------------------------------- *)

(*
   In this section we write functions to work flexibly with temperatures.
   A value of type "temp" is effectively a number 'tagged' with a C or F
   depending on if it is meant to be Celsius or Fahrenheit.
   The conversion between the two is the familiar formula: F = 1.8 * C + 32
*)
type temp = C of float | F of float

(*
   Write a function `to_f` that takes as input a value of type "temp" and
   returns the temperature measured in Fahrenheit.
   Note: The operators for floating point arithmetic have a dot following
   them to distinguish from the integer ones. For example "2.1 +. 5.2"
   Type: temp -> float
*)

let to_f (degrees : temp) : float =
   match degrees with
   | F heit-> heit
   | C sius -> 1.8 *. sius +. 32.0

(*
   Write a function `temp_compare` that takes as input a pair of temperatures and
   "compares" them, returning 1 if the first temperature is higher, 0 if they are
   equal and -1 if the second temperature is higher.
   Type: temp * temp -> int
*)

let temp_compare (temp1, temp2 : temp * temp) :int =
   let t1 = to_f(temp1) in
   let t2 = to_f(temp2)
   in if t1 > t2
      then 1
      else if t1 < t2
           then -1
           else 0

(*
   Write a function `string_of_temp` that takes as input a temperature and
   returns a string representing that temperature. For instance 23.2 Fahrenheit
   should print as "23.2F" while 23.2 Celcius as "23.2C". Look in the Pervasives
   module in the string conversions section for a function converting floats
   to strings.
   Type: temp -> string
*)

let string_of_temp (degrees : temp) : string = 
   match degrees with
   | F heit -> string_of_float heit ^ "F"
   | C sius -> string_of_float sius ^ "C"


(*
   Write a function `max_temp` that takes as input a list of temperatures and
   returns the largest one. It should raise an exception `Failure "max_temp"`
   if the list is empty.
   Type: temp list -> temp
*)

let rec max_temp (lst_temps : temp list) : temp = 
  if lst_temps = []
  then failwith("max_temp")
  else match lst_temps with
       | [] -> F 0.0
       | temp :: rest ->
         if to_f (temp) > to_f (max_temp (rest))
         then temp
         else max_temp (rest)



(*
   Write a function `max_temp2` that behaves like `max_temp` but where all the
   recursive calls are tail calls. You will likely need to define an auxiliary
   function and use state recursion.
*)

let max_temp2 (lst_temps2 : temp list) : temp =
   if lst_temps2 = []
   then failwith("max_temp2")
   else let rec aux (max_sofar, remaining : temp * temp list) =
         match remaining with
         | [] -> max_sofar
         | p1 :: rest -> 
            match p1 with
            | temp -> 
               if to_f (temp) > to_f (max_sofar)
               then aux (temp, rest)
               else aux (max_sofar, rest) 

      in aux (F 0.0, lst_temps2) 
