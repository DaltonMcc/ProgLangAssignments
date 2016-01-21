(* Add your own tests. Make sure to pay attention to edge cases. *)

(*--------------------------------------------*)
let t1a = fixLastTwo (5, 1, 2) = (5, 1, 2)
let t1b = fixLastTwo (5, 2, 1) = (5, 1, 2)
let t1c = fixLastTwo (0, 8, 9) = (0, 8, 9)
let t1d = fixLastTwo (8, 9, 3) = (8, 3, 9)
let t1e = fixLastTwo (8, 3, 9) = (8, 3, 9)
let t1f = fixLastTwo (2, 12, 0) = (2, 0, 12)


(*--------------------------------------------*)
let t2a = order (2, 5, 3) = (2, 3, 5)
let t2b = order (5, 3, 2) = (2, 3, 5)
let t2c = order (1, 2, 3) = (1, 2, 3)
let t2d = order (8, 3, 10) = (3, 8, 10)
let t2e = order (1, 5, 2) = (1, 2, 5)
let t2f = order (10, 9, 4) = (4, 9, 10)
let t2g = order (11, 8, 16) = (8, 11, 16)


(*--------------------------------------------*)
let t3a = distance (6, 3) = 3
let t3b = distance (10, 20) = 10
let t3c = distance (0, 5) = 5
let t3d = distance (1, 21) = 20
let t3e = distance (37, 19) = 18
let t3f = distance (1, 1) = 0
let t3g = distance (8, 9) = 1


(*--------------------------------------------*)
let t4a = greeting (23, "Pete") = "Greetings Pete, you are 23 years old!"
let t4b = greeting (1, "jack") = "Greetings jack, you are 1 years old!"
let t4c = greeting (0, "Ham") = "Greetings Ham, you are 0 years old!"
let t4d = greeting (-2, "Maria") = "Greetings Maria, you are -2 years old!"
let t4e = greeting (14, "Jacob") = "Greetings Jacob, you are 14 years old!"
let t4f = greeting (21, "Dalton") = "Greetings Dalton, you are 21 years old!"


(*--------------------------------------------*)
let t5a = greeting2 (0, "Jackson") = "Greetings Jackson, you are not born yet!"
let t5b = greeting2 (5, "Hunter") = "Greetings Hunter, you are a youngster!"
let t5c = greeting2 (21, "John") = "Greetings John, you are young at heart!"
let t5d = greeting2 (20, "Bob") = "Greetings Bob, you are a youngster!"
let t5e = greeting2 (-3, "Lisa") = "Greetings Lisa, you are not born yet!"
let t5f = greeting2 (100, "Kelly") = "Greetings Kelly, you are young at heart!"
let t5g = greeting2 (1, "Mark") = "Greetings Mark, you are a youngster!"


(*--------------------------------------------*)
let t6a = tooShort (4, "tree") = false
let t6b = tooShort (1, "Hood") = false
let t6c = tooShort (8, "Match") = true
let t6d = tooShort (4, "Table") = false
let t6e = tooShort (100, "Test") = true
let t6f = tooShort (7, "Caesar") = true


(*--------------------------------------------*)
let t7a = totalLength ("you", "me") = 5
let t7b = totalLength ("Alfred", "Penny") = 11
let t7c = totalLength ("Length", "Long") = 10
let t7d = totalLength ("A", "B") = 2
let t7e = totalLength ("Zulu", "Utop") = 8


(*--------------------------------------------*)
let t8a = orderedByLength ("long", "one", "at") = false
let t8b = orderedByLength ("at", "one", "long") = true
let t8c = orderedByLength ("at", "a", "John") = false
let t8d = orderedByLength ("Bob", "Henry", "Al") = false
let t8e = orderedByLength ("a", "aa", "aaa") = true


(*--------------------------------------------*)
let t9a = prodInRange (3, 5) = true
let t9b = prodInRange (2, 10) = false 
let t9c = prodInRange (1, 10) = false 
let t9d = prodInRange (2, 11) = false
let t9e = prodInRange (3, 6) = true
let t9f = prodInRange (4, 4) = true
let t9g = prodInRange (5, 5) = false
