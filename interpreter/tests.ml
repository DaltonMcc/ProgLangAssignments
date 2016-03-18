open Types

(* You can test expressions of type resultS or resultC and how they are evaluated *)
(* These will only work once you have compiled types.ml *)

(* This is the one kind of test you can write. *)
let t0a = evaluate (NumC 2.3) = Num 2.3


(* You can also use interp directly to specify a custom environment. *)
let t0b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (NumC 2.3) = Num 2.3

let t1b = let env1 = bind "y" (Bool true) empty
          in interp env1 (BoolC true) = Bool true

let t2b = let env1 = bind "z" (Bool false) empty
          in interp env1 (BoolC false) = Bool false

let t3b = IfC (BoolC true, NumC 2.3, NumC 4.3) = Num 2.3
let t4b = IfC (BoolC false, NumC 2.3, NumC 4.3) = Num 4.3
let t5b = IfC ((IfC (1 < 2, BoolC true, BoolC false)), NumC 2.3, NumC 4.3) = Num 2.3
let t6b = IfC ((IfC (1 > 2, BoolC true, BoolC false)), NumC 2.3, NumC 4.3) = Num 4.3
let t7b = try (IfC (4, NumC 4, NumC 0) -> raise (Failure ""))); true) with
          | _ -> false


let t8b = Not (BoolC true) = false
let t9b = Or ((NumC 1 < NumC 2), (NumC 1 > NumC 2)) = BoolC true
let t10b = Or ((NumC 1 > NumC 2), (NumC 1 < NumC 2)) = BoolC false
let t11b = And (NumC 1, NumC 2)
let t12b = And (BoolC false, BoolC true)

let t13b = Plus (NumC 2 PLUS NumC 3) = Num 5

(* You can also test desugar to make sure it behaves properly. *)
let t0c = desugar (NumS 2.3) = NumC 2.3
let t1c = desugar (BoolS false) = BoolC false
let t2c = desugar (BoolS true) = BoolC true

(* Or you can combine with evaluate to get to the final value. *)
let t0d = evaluate (desugar (NumS 2.3)) = Num 2.3
