exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float
			| BoolS of bool
      | IfS of exprS * exprS * exprS 
      | OrS of exprS * exprS
      | AndS of exprS * exprS
      | NotS of exprS
      | ArithS of string * exprS * exprS
      | CompS of string * exprS * exprS


(* You will need to add more cases here. *)
type exprC = NumC of float
			| BoolC of bool
      | IfC of exprC * exprC * exprC
      | ArithC of string * exprC * exprC
      | CompC of string * exprC * exprC


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
(*
   Implement a new "helper method" called `arithEval` with type `string -> value -> value -> value` 
   that takes the operator and two values. If the two values are not both `Num`s then it should raise 
   an interpreter exception. If they are then it should perform the appropriate operation and 
   return a "value" of the result. The operators we will allow will be "+", "-", "*" and "/". 
   It should throw an interpreter error if the operator symbol is not one of these. 
   It should also throw an interpreter error about division by zero if the operator is division and the denominator is 0. 
   Remember that in our arithmetic world, all numbers are floating point numbers.
*)

let arithEval (op, v1, v2) =
  match v1 with
  | NumC i -> (match v2 with
              | NumC e -> (match op with
                          | "+" -> string_of_float (i+e)
                          | "-" -> string_of_float (i-e)
                          | "*" -> string_of_float (i*e)
                          | "/" -> if e = 0
                                   then raise Failure ("divide by zero")
                                   else string_of_float (i/e)
              | _ -> raise Failure ("Interp")
                          )
              )
  | _ -> raise Failure ("Interp")



  let compEval (comp, v1, v2) =
  match v1 with
  | NumC i -> (match v2 with
              | NumC e -> (match comp with
                          | ">" -> i > e
                          | "<" -> i < e
                          | ">=" -> i >= e
                          | "<=" -> i <= e
                          | _ -> raise Failure ("no comparison operator")
              | _ -> raise Failure ("Interp")
                          )
              )
  | _ -> raise Failure ("Interp")

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
  | ArithS (str, e1, e2) -> ArithC (str, desugar e1, desugar e2)




(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
  | BoolC i 	    -> Bool i
  | IfC (f, th, els) -> let con_test = interp f in
                    if con_test = Bool true || con_test = Bool false
                    then raise Failure ("Interp")
                    else match con_test with
                        | Bool true -> interp th
                        | Bool false -> interp els
  | ArithC (str, e1, e2) -> let e_op1 = interp e1 in
                            let e_op2 = interp e2 in
                              if e_op1 > e_op2
                              then arithEval (str, e1, e2)
                              else arithEval (str, e2, e1)
  | CompC (str, e1, e2) ->  let e1_comp = interp e1 in
                            let e2_comp = interp e2 in
                            compEval (str, e1_comp, e2_comp)




(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []




(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
