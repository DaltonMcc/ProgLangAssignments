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
let t4c = valid_game [(Paper, Paper); (Paper, Paper)] = false
let t4d = valid_game [(Paper, Paper); (Paper, Rock)] = true 
let t4e = valid_game [(Scissors, Rock)] = true
let t4f = valid_game [(Paper, Scissors)] = true
let t4g = valid_game [(Rock, Paper); (Rock, Scissors)] = false


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
let t6b = to_f (F 5.5) = 5.5
let t6c = to_f (F 6.0) = 6.0
let t6d = to_f (C 0.0) = 32.0
let t6e = to_f (C 5.2) = 41.36
let t6f = to_f (C 1.4) = 34.52


(*--------------------------------------------*)
let t7a = temp_compare (F 2.3, F 4.5) = -1
let t7b = temp_compare (C 4.4, C 2.0) = 1
let t7c = temp_compare (F 32.0, C 0.0) = 0
let t7d = temp_compare (C 3.1, F 37.76) = -1
let t7e = temp_compare (C 3.3, C 3.3) = 0 
let t7f = temp_compare (F 48.8, C 5.1) = 1
let t7g = temp_compare (F 21.1, F 21.2) = -1

(*--------------------------------------------*)
let t8a = string_of_temp (C 2.3) = "2.3C"
let t8b = string_of_temp (C 8.2) = "8.2C"
let t8c = string_of_temp (C 6.6) = "6.6C"
let t8d = string_of_temp (F 3.9) = "3.9F"
let t8e = string_of_temp (F 21.1) = "21.1F"
let t8f = string_of_temp (F 45.3) = "45.3F"


(*--------------------------------------------*)
let t9a = max_temp [F 2.1; C 2.1] = C 2.1
let t9b = max_temp [F 3.3; F 3.2; F 3.1; F 3.6] = F 3.6 
let t9c = max_temp [C 6.6; C 5.6; C 4.6] = C 6.6
let t9d = max_temp [C 1.5; F 21.4; F 45.1; C 7.2] = F 45.1
let t9e = max_temp [F 7.4; F 41.9; C 5.6; F 25.7; C 5.5] = C 5.6
let t9f = max_temp [F 66.2; C 19.1] = C 19.1
let t9g = max_temp [C 1.1] = C 1.1
let t9h = try (max_temp []; false) with
            | Failure "getnth" -> true
            | _ -> false


(*--------------------------------------------*)
let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1
let t10b = max_temp2 [F 3.3; F 3.2; F 3.1; F 3.6] = F 3.6 
let t10c = max_temp2 [C 6.6; C 5.6; C 4.6] = C 6.6
let t10d = max_temp2 [C 1.5; F 21.4; F 45.1; C 7.2] = F 45.1
let t10e = max_temp2 [F 7.4; F 41.9; C 5.6; F 25.7; C 5.5] = C 5.6
let t10f = max_temp2 [F 66.2; C 19.1] = C 19.1
let t10g = max_temp2 [C 1.1] = C 1.1
let t10h = try (max_temp2 []; false) with
            | Failure "getnth" -> true
            | _ -> false

