(*-----------------R/P/S---------------------*)


(*-------------------------------------------*)
let t1a = result (Rock, Paper) = SndWin
let t1b = result (Rock, Scissors) = FstWin
let t1c = result (Rock, Rock) = Tie
let t1d = result (Paper, Paper) = Tie
let t1e = result (Paper, Scissors) = SndWin
let t1f = result (Paper, Rock) = FstWin
let t1g = result (Scissors, Paper) = FstWin
let t1h = result (Scissors, Scissors) = Tie
let t1i = result (Scissors, Rock) = SndWin

(*--------------------------------------------*)
let t2a = is_tie (Rock, Rock) = true
let t2b = is_tie (Rock, Paper) = false
let t2c = is_tie (Rock, Scissors) = false
let t2d = is_tie (Paper, Rock) = false
let t2e = is_tie (Paper, Paper) = true
let t2f = is_tie (Paper, Scissors) = false
let t2g = is_tie (Scissors, Rock) = false
let t2h = is_tie (Scissors, Paper) = false
let t2i = is_tie (Scissors, Scissors) = true

(*--------------------------------------------*)
let t3a = game_from_plays ([Rock; Paper; Rock], [Scissors; Rock; Rock]) = [(Rock, Scissors); (Paper, Rock); (Rock, Rock)]

let t3b = game_from_plays ([Rock; Paper], [Rock; Rock]) = [(Rock, Rock); (Paper, Rock)]

let t3c = game_from_plays ([Scissors; Paper; Rock], [Scissors; Rock]) = [(Scissors, Scissors); (Paper, Rock)]

let t3d = game_from_plays ([Rock], [Paper; Rock]) = [(Rock, Paper)]

let t3e = game_from_plays ([Rock; Rock; Rock;], [Rock; Rock; Paper]) = [(Rock, Rock); (Rock, Rock); (Rock, Paper)]


(*--------------------------------------------*)
let t4a = valid_game [(Rock, Scissors)] = true
let t4b = valid_game [(Rock, Rock)] = false
let t4c = valid_game [(Rock, )] = false
let t4d = valid_game [(Paper, Paper); (Paper, Paper)] = false
let t4e = valid_game [(Paper, Paper); (Paper, Rock)] = true 
let t4f = valid_game [(Scissors, Rock)] = true
let t4g = valid_game [(Paper, Scissors)] = true
let t4h = valid_game [(Rock, Paper); (Rock, Scissors)] = false


(*--------------------------------------------*)
let t5a = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin
let t5b = play_game [(Rock, Paper)] = SndWin
let t5c = play_game [(Paper, Scissors)] = SndWin
let t5d = play_game [(Paper, Paper); (Paper, Paper); (Paper, Rock)] = FstWin
let t5e = play_game [(Scissors, Rock)] = SndWin
let t5f = play_game [(Rock, Rock); (Paper, Paper); (Scissors, Scissors); (Paper, Rock)] = FstWin
let t5g = play_game [(Scissors, Paper)] = FstWin



(*-------------------TEMP---------------------*)




(*--------------------------------------------*)
let t6a = to_f (F 2.3) = 2.3


(*--------------------------------------------*)
let t7a = temp_compare (F 2.3, F 4.5) = -1


(*--------------------------------------------*)
let t8a = string_of_temp (C 2.3) = "2.3C"


(*--------------------------------------------*)
let t9a = max_temp [F 2.1; C 2.1] = C 2.1


(*--------------------------------------------*)
let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1

