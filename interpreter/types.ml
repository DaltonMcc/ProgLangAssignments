exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float
			| BoolS of bool
      | IfS of exprS * exprS * exprS 
      | OrS of exprS * exprS
      | AndS of exprS * exprS
      | NotS of exprS


(* You will need to add more cases here. *)
type exprC = NumC of float
			| BoolC of bool
      | IfC of exprC * exprC * exprC


(* You will need to add more cases here. *)
type value = Num of float
			| Bool of bool



type 'a env = (string * 'a) list
let empty = []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(*
   HELPER METHODS
   You may be asked to add methods here. You may also choose to add your own
   helper methods here.
*)
(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | NumS i        -> NumC i
  | BoolS i  	    -> BoolC i
  | IfS (x, y, z) -> IfC (desugar x, desugar y, desugar z)
  | OrS (e1, e2)  -> if desugar e1 = BoolC true
                     then BoolC true
                     else if desugar e2 = BoolC true
                          then BoolC true
                          else BoolC false
  | NotS (e)      -> if desugar e = BoolC true
                     then BoolC false
                     else BoolC true
  | AndS (e1, e2) -> if (desugar e1) = BoolC true
                     then if (desugar e2) = BoolC true
                          then BoolC true
                          else BoolC false
                     else BoolC false





(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
  | BoolC i 	    -> Bool i
  | IfC (x, y, z) -> let con_test = interp x in
                    if con_test = Bool true || con_test = Bool false
                    then raise Failure ("Interp")
                    else match con_test with
                        | Bool true -> interp y
                        | Bool false -> interp z





(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []




(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
