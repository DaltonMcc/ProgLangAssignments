(*--------------- TESTS_assignment2 -----------------*)
(*---------------------------------------------------*)

(*---------------------------------------------------*)
let t1a = getnth (3, ["hi"; "there"; "you"]) = "you"
let t1b = try (getnth (3, ["hi"; "there"]); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1c = getnth (1, ["first"; "second"; "third"]) = "first"
let t1d = getnth (4, ["first"; "second"; "third"; "fourth"]) = "fourth"
let t1e = try (getnth (5, ["first"; "second"; "third"; "fourth"]); false) with
			| Failure "getnth" -> true
			| _ -> false
let t1f = try (getnth (1, []); false) with
			| Failure "getnth" -> true
			| _ -> false
let t1g = try (getnth (8, ["first"; "second"; "third"; "fourth"]); false) with
			| Failure "getnth" -> true
			| _ -> false
let t1h = getnth (3, ["first"; "second"; "third"; "fourth"]) = "third"
let t1h = getnth (8, ["first"; "second"; "third"; "fourth"; "fifth"; "sixth"; "seventh"; "eighth"]) = "eighth" 


(*---------------------------------------------------*)
let t2a = lookup ("you", []) = None
let t2b = lookup ("you", [("him", 2); ("you", 3)]) = Some 3
let t2c = lookup ("last", [("first", 1); ("second", 2); ("third", 3); ("last", 4)]) = Some 4
let t2d = lookup ("last", [("first", 1); ("second", 2); ("third", 3); ("fourth", 4); ("fifth", 5)]) = None
let t2e = lookup ("first", [("first", 1); ("second", 2); ("third", 3); ("last", 4)]) = Some 1
let t2f = lookup ("Dalton", [("Evan", 1); ("Luke", 2); ("Dalton", 3); ("Flynn", 4); ("Noah", 5)]) = Some 3


(*---------------------------------------------------*)
let t3a = inPairs ([1; 2; 3; 4; 5]) = [(1, 2); (3, 4)]
let t3b = inPairs ([1; 2; 3; 4; 5; 6]) = [(1, 2); (3, 4); (5, 6)]
let t3c = inPairs ([1; 2; 3]) = [(1, 2)] (*odd #, 3 is left out*)
let t3d = inPairs ([1; 2]) = [(1, 2)]
let t3e = inPairs ([5; 6; 7; 8; 9; 10; 11]) = [(5, 6); (7, 8); (9, 10)] (*odd #, 11 is left out*)
let t3f = inPairs ([5; 6; 7; 8; 9; 10; 11; 12]) = [(5, 6); (7, 8); (9, 10); (11, 12)]
let t3g = inPairs ([1]) = [] (*odd #, no pairs*)
let t3h = inPairs ([]) = [] (*nothing*)


(*---------------------------------------------------*)
let t4a = flatten [[1; 2; 3]; []; [4; 5]; [6]] = [1; 2; 3; 4; 5; 6]
let t4b = flatten [[1; 2]; []; [4; 5; 6]; []; []] = [1; 2; 4; 5; 6]
let t4c = flatten [[]; []; []; []; []] = []
let t4d = flatten [[]; []; []; [1; 5]] = [1; 5]
let t4e = flatten [[1; 3; 4]; [2; 2]; []; [1; 2]] = [1; 3; 4; 2; 2; 1; 2]


(*---------------------------------------------------*)
let t5a = remove (3, [3; 4; 3; 1]) = [4; 1]
let t5b = remove (1, [1; 1; 1; 1]) = []
let t5c = remove (1, [2; 3; 1; 1; 5]) = [2; 3; 5]
let t5d = remove (5, [1; 2; 3; 4]) = [1; 2; 3; 4]
let t5e = remove (5, [1; 2; 3; 5; 4; 5; 6; 5; 5; 7]) = [1; 2; 3; 4; 6; 7]
let t5f = remove (9, [1; 9; 2; 9; 3; 9; 4; 9; 5; 9;]) = [1; 2; 3; 4; 5]


(*---------------------------------------------------*)
let t6a = removeDups [4; 1; 2; 1; 4; 5; 20] = [4; 1; 2; 5; 20]
let t6b = removeDups [1; 1; 2; 2; 3; 3; 4; 4] = [1; 2; 3; 4]
let t6b = removeDups [1; 1; 2; 1; 1] = [1; 2]
let t6b = removeDups [1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6] = [1; 2; 3; 4; 5; 6]
let t6b = removeDups [1; 2; 3; 4; 5] = [1; 2; 3; 4; 5]


(*---------------------------------------------------*)
let t7a = collateSome ([Some 1; None; Some 2; Some 1; None; Some 3]) = [1; 2; 1; 3]
let t7b = collateSome ([Some 2; Some 2; None; Some 3]) = [2; 2; 3]
let t7c = collateSome ([None; None; None]) = []
let t7d = collateSome ([None; Some 1; None; Some 2; None]) = [1; 2]
let t7e = collateSome ([Some 5; Some 3; Some 4; None; None; Some 2; Some 1]) = [5; 3; 4; 2; 1]
let t7f = collateSome ([None; Some 1; Some 2; None; None; None; None; Some 3]) = [1; 2; 3]



(*---------------------------------------------------*)
let t8a = unzip2 [(1, 2); (3, 4); (5, 6)] = ([1; 3; 5], [2; 4; 6])
let t8b = unzip [(1,2)] = ([1], [2])
let t8b = unzip [(1, 2); (3, 4)] = ([1; 3], [2; 4])
let t8b = unzip [(1, 2); (3, 4); (5, 5)] = ([1; 3; 5], [2; 4; 5])
let t8b = unzip [(9, 8); (7, 6); (5, 4); (3, 2)] = ([9; 7; 5; 3], [8; 6; 4; 2])
let t8b = unzip [] = ()


(*---------------------------------------------------*)
let t9a = makeChange (20, [8; 3; 2]) = Some [8; 8; 2; 2]
let t9b = makeChange (20, [8; 3]) = Some [8; 3; 3; 3; 3]
let t9c = makeChange (20, [13; 11]) = None
let t9d = makeChange (50, [45; 5; 1]) = Some [45; 5]
let t9e = makeChange (11, [10; 3; 2]) = Some [3; 3; 3; 2]
let t9f = makeChange (25, [25, 6, 5, 2]) = Some [25]
let t9g = makeChange (25, [22, 20, 10, 2, 1]) = Some [22; 2; 1]
let t9h = makeChange (25, [22, 20, 10]) = None
let t9i = makeChange (24, [22, 20, 2]) = Some [2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2]
let t9j = makeChange (25, [24, 20, 10, 4, 3]) = Some [10; 3; 3; 3; 3; 3]

