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
      | EqS of exprS * exprS


(* You will need to add more cases here. *)
type exprC = NumC of float
			| BoolC of bool
      | IfC of exprC * exprC * exprC
      | ArithC of string * exprC * exprC
      | CompC of string * exprC * exprC
      | EqC of exprC * exprC


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

let arithEval (op, v1, v2) =
  match v1 with
  | Num i -> (match v2 with
              | Num e -> (match op with
                          | ("+" : string) -> Num (i +. e)
                          | ("-" : string) -> Num (i -. e)
                          | ("*" : string) -> Num (i *. e)
                          | ("/" : string) -> if e = 0.0
                                   then raise (Interp ("divide by zero"))
                                   else Num (i /. e))
                          | _ -> raise (Interp ("No Operator"))
              | Bool _ | _ -> raise (Interp ("Second Value is not Num"))
                          
              )
  | Bool _ | _ -> raise (Interp ("First Value is not Num"))



let compEval (comp, v1, v2) =
  match v1 with
  | Num i -> (match v2 with
              | Num e -> (match comp with
                          | (">" : string) -> Bool (i > e)
                          | ("<"  : string)-> Bool (i < e)
                          | (">=" : string)-> Bool (i >= e)
                          | ("<=" : string) -> Bool (i <= e)
                          | _ -> raise (Interp ("no comparison operator"))
                        )
              | Bool _ | _ -> raise (Interp ("Second Value is not Num"))         
              )
  | _ -> raise (Interp ("First Value is not Num"))



  let eqEval (v1, v2) = 
  match v1 with
  | NumC i -> (match v2 with
                | NumC e -> Bool (i = e)
                | _ -> Bool false
                )
  | BoolC i -> (match v2 with
                | BoolC e -> Bool (i = e)
                | _ -> Bool false
                )
  | _ -> Bool false
  

(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | NumS i        -> NumC i
  | BoolS i  	    -> BoolC i
  | IfS (x, y, z) -> IfC (desugar x, desugar y, desugar z)
  | OrS (e1, e2)  -> IfC (desugar e1, BoolC true, (IfC (desugar e2, BoolC true, BoolC false)))
  | NotS (e)      -> IfC (desugar e, BoolC false, BoolC true)
  | AndS (e1, e2) -> IfC (desugar e1, (IfC (desugar e2, BoolC true, BoolC false)), BoolC false)
  | ArithS (str, e1, e2) -> ArithC (str, desugar e1, desugar e2)




(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = 
  match r with
  | NumC i        -> Num i
  | BoolC i 	    -> Bool i

  | IfC (f, th, els) -> (match f with
                        | BoolC true  -> (match th with
                                         | NumC i -> Num i
                                         | BoolC i -> Bool i)
                        | BoolC false -> (match els with
                                         | NumC e -> Num e
                                         | BoolC e -> Bool e)
                        (*| IfC (f2, th2, els2) -> interp (IfC (f2, th2, els2))*)
                        | _ -> raise (Interp ("Not Boolean"))
                        )
                        

  | ArithC (str, e1, e2) -> let op1 = 
                            (match e1 with
                            | NumC i -> Num i) in
                                let op2 = 
                                (match e2 with
                                | NumC e -> Num e)
                            in (arithEval (str, op1, op2))

  | CompC (str, e1, e2) ->  let e1_comp = 
                            (match e1 with
                            | NumC i -> Num i) in
                                let e2_comp = 
                                (match e2 with
                                | NumC e -> Num e)
                            in compEval (str, e1_comp, e2_comp)

  | EqC (e1, e2)        -> (*let q1 = 
                            (match e1 with
                            | NumC i -> Num i
                            | BoolC i -> Bool i) in
                                let q2 = 
                                (match e2 with
                                | NumC e -> Num e
                                | BoolC e -> Bool e)
                            in eqEval (q1, q2*)
                            eqEval (e1, e2)




(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []




(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool i          -> string_of_bool i
