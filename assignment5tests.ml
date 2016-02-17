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
let t9d = count_vars (Add (Var, Int 1)) = 1
let t9e = count_vars (Mul (Int 1, Int 1)) = 0
let t9f = count_vars (Sub (Var, Var)) = 2 


(*----------------------------------------------*)
let t10a = calc_eval (Add (Var, Int 2), 3) = 5
let t10b = calc_eval (Sub (Var, Int 5), 10) = 5
let t10c = calc_eval (Mul (Var, Int 2), 4) = 8
let t10d = calc_eval (Add (Int 7, Var), 7) = 14 
let t10e = calc_eval (Sub (Int 100, Var), 1) = 99
let t10f = calc_eval (Mul (Int 10, Var), 3) = 30


(*----------------------------------------------*)
let t11a = func_of_calc (Add (Var, Int 2)) 3 = 5
let t11b = func_of_calc (Sub (Var, Int 5)) 6 = 1 
let t11c = func_of_calc (Mul (Int 2, Var)) 5 = 10
let t11d = func_of_calc (Add (Var, Int 4)) 4 = 8 
let t11e = func_of_calc (Sub (Int 15, Var)) 8 = 7
let t11f = func_of_calc (Mul (Int 3, Var)) 3 = 9 


(*----------------------------------------------*)
let t12a = subst (Add (Var, Int 1), Mul (Var, Var)) =
                Mul (Add (Var, Int 1), Add (Var, Int 1))
let t12b = subst (Sub (Var, Var), Add (Int 1, Var)) = 
				Add (Sub (Var, Var), Sub (Var, Var)) 
let t12c = subst (Mul (Var, Int 5), Sub (Var, Var)) = 
				Sub (Mul (Var, Int 5), Mul (Var, Int 5))
let t12d = subst (Add (Int 5, Int 5), Mul (Var, Var)) = 
				Mul (Add (Int 5, Int 5), Add (Int 5, Int 5))
let t12e = subst (Sub (Var, Var), Sub (Int 5, Var)) = 
				Sub (Sub (Var, Var), Sub (Var, Var))
let t12f = subst (Mul (Int 1, Int 6), Add (Var, Int 4)) = 
				Add (Mul (Int 1, Int 6), Mul (Int 1, Int 6)) 


(*----------------------------------------------*)
let t13a = power 3 = Mul (Mul (Var, Var), Var)
let t13b = power 2 = Mul (Var, Var)
let t13c = power 1 = Var
let t13d = power 4 = Mul (Mul (Mul (Var, Var), Var), Var)
let t13e = power 5 = Mul (Mul (Mul (Mul (Var, Var), Var), Var), Var)


(*----------------------------------------------*)
let t14a = term (2, 1) = Mul(Int 2, Var)
let t14b = term (1, 4) = Power (Var, Int 4)
let t14c = term (0, 2) = Mul (Int 0, Var)
let t14d = term (2, 2) = Mul (Int 2, Power (Var, Int 2))
let t14e = term (3, 2) = Mul (Int 3, Power (Var, Int 2))
let t14f = term (5, 3) = Mul (Int 5, Power (Var, Int 3))


(*----------------------------------------------*)
let t15a = poly [(2, 1); (1, 4)] = Add (term (2, 1), term (1, 4))
let t15b = poly [(2, 1); (0, 2); (1, 4)] = Add (term (2, 1), term (1, 4))
let t15c = poly [(0, 1); (0, 4)] = Int 0
let t15d = poly [(0, 1)] = Int 0
let t15e = poly [] = Int 0
let t15f = poly [(2, 1); (0, 1)] = term (2, 1)
let t15g = poly [(0, 1); (2, 1)] = term (2, 1)


(*----------------------------------------------*)
let t16a = simplify (Add (Int 0, Var)) = Var
let t16b = simplify (Add (Int 3, Int 4)) = Int 7
let t16c = calc_eval (simplify (poly [(2, 1); (1, 0)]), 3) = 7
