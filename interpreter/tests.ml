open Types

(* You can test expressions of type resultS or resultC and how they are evaluated *)
(* These will only work once you have compiled types.ml *)

(* This is the one kind of test you can write. *)
let t0a = evaluate (NumC 2.3) = Num 2.3


(* You can also use interp directly to specify a custom environment. *)  (*  #use "tests.ml";;  *)
let t0b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (NumC 2.3) = Num 2.3

let t1b = let env1 = bind "y" (Bool true) empty
          in interp env1 (BoolC true) = Bool true

let t2b = let env1 = bind "z" (Bool false) empty
          in interp env1 (BoolC false) = Bool false

let t3b = evaluate (IfC (BoolC true, NumC 2.3, NumC 4.3)) = Num 2.3
let t4b = evaluate (IfC (BoolC false, NumC 2.3, NumC 4.3)) = Num 4.3
(*let try_true = evaluate (IfC (BoolC true, BoolC true, BoolC false))*)
let t5b = evaluate (IfC (BoolC true, NumC 2.3, NumC 4.3)) = Num 2.3
let t6b = evaluate (IfC (BoolC false, NumC 2.3, NumC 4.3)) = Num 4.3
(*let t7b = try (evaluate (IfC (4, NumC 4, NumC 0))) -> raise (Failure "Not Boolean"))); true) with
          | _ -> false*)


let t8b = evaluate (desugar (NotS (BoolS true))) = Bool false
let t9b = evaluate (desugar (OrS ((BoolS true), (BoolS false)))) = Bool true
(* let t10b = evaluate (desugar (OrS ((BoolS false), (BoolS false)))) = Bool false *)
let t11b = evaluate (desugar (AndS (NumS 1.1, NumS 2.2)))
let t12b = evaluate (desugar (AndS (BoolS false, BoolS true)))

let t13b = Plus (NumC 2 + NumC 3) = Num 5

(* You can also test desugar to make sure it behaves properly. *)
let t0c = desugar (NumS 2.3) = NumC 2.3
let t1c = desugar (BoolS false) = BoolC false
let t2c = desugar (BoolS true) = BoolC true

(* Or you can combine with evaluate to get to the final value. *)
let t0d = evaluate (desugar (NumS 2.3)) = Num 2.3
