(* CALCULATIONS *)


(*----------------------------------------------*)
let t8a = has_vars (Add (Var, Int 2))
let t8b = not (has_vars (Add (Int 1, Int 2)))
let t8c = has_vars (Sub (Var, Int 1))
let t8d = has_vars (Add (Var, Var))
let t8e = not (has_vars (Sub (Int 2, Int 1)))
let t8f = not (has_vars (Add (Int 5, Int 5)))
let t8g = has_vars (Mul (Var, Int 1))

(*----------------------------------------------*)
let t9a = count_vars (Add (Var, Int 2)) = 1
let t9b = count_vars (Add (Int 1, Int 2)) = 0
let t9c = count_vars (Sub (Var, Var)) = 2 
let t9d = count_vars (Add ((Sub (Var, Int 1), Var))) = 2
let t9e = count_vars (Mul (Int 1, Int 1)) = 0
let t9f = count_vars (Sub (Add (Var, Var), Var)) = 3 


(*----------------------------------------------*)
let t10a = calc_eval (Add (Var, Int 2), 3) = 5
let t10b = calc_eval (Sub (Var, Int 5) 10) = 5
let t10c = calc_eval (Mul (Var, 2), 4) = 8
let t10d = calc_eval (Add (Var, Var), 7) = 14 
let t10e = calc_eval (Sub (Int 100, Var), 1) = 99
let t10f = calc_eval (Mul (Int 10, Var), 3) = 30


(*----------------------------------------------*)
let t11a = func_of_calc (Add (Var, Int 2)) 3 = 5
let t11b = func_of_calc (Sub (Var, Int 5)) 6 = 1 
let t11c = func_of_calc (Mul (Int 2, Var)) 5 = 10
let t11d = func_of_calc (Add (Var, Var)) 4 = 8 
let t11e = func_of_calc (Sub (Int 15, Var)) 8 = 7
let t11f = func_of_calc (Mul (Var, Var)) 3 = 9 


(*----------------------------------------------*)
let t12a = subst (Add (Var, Int 1), Mul (Var, Var)) =
                Mul (Add (Var, Int 1), Add (Var, Int 1))


(*----------------------------------------------*)
let t13a = power 3 = Mul (Mul (Var, Var), Var)


(*----------------------------------------------*)
let t14a = term (2, 1) = Mul(Int 2, Var)


(*----------------------------------------------*)
let t15a = poly [(2, 1); (1, 4)] = Add (term (2, 1), term (1, 4))


(*----------------------------------------------*)
let t16a = simplify (Add (Int 0, Var)) = Var
let t16b = simplify (Add (Int 3, Int 4)) = Int 7
let t16c = calc_eval (simplify (poly [(2, 1); (1, 0)]), 3) = 7
