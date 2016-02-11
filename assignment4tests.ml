(* THUNKS *)
(* This complicated test ensures you don't call the thunk too soon *)

(*-----------------------------------------------*)
let t1a = let f = fun () -> raise (Failure "")
          in try (try (thunk f) with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false

let t1b = (thunk (fun () -> 5)) () = 5
let t1c = (thunk (fun () -> "test")) () = "test"
let t1d = (thunk (fun () -> true)) () = true
let t1e = (thunk (fun () -> (2, "two"))) () = (2, "two")
let t1f = (thunk (fun () -> Some 7)) () = Some 7
let t1g = (thunk (fun () -> [1; 2; 3])) () = [1; 2; 3]


(*-----------------------------------------------*)
let t2a = (thunk_of_value 4) () = 4
let t2b = (thunk_of_value "test") () = "test"
let t2c = (thunk_of_value true) () = true
let t2d = (thunk_of_value (2, "two")) () = (2, "two")
let t2e = (thunk_of_value [1; 2; 3]) () = [1; 2; 3]


(*-----------------------------------------------*)
let t3a = try (try (thunk_of_eval ((fun x -> raise (Failure "")), 4))
               with Failure "" -> (fun () -> false)) ()
          with Failure "" -> true
             | _ -> false
let t3b = thunk_of_eval ((fun x -> x + 1), 5) () = 6
let t3c = thunk_of_eval ((fun x -> x ^ " tests"), "more") () = "more tests"
let t3d = thunk_of_eval ((fun x -> x :: [3; 4; 5]), [1; 2]) () = [1; 2; 3; 4; 5]
let t3e = thunk_of_eval ((fun x -> x * x), 5) () = 10
let t3f = thunk_of_eval ((fun x -> x ^ x), "Lu") () = "LuLu"


(*-----------------------------------------------*)
let t4a = try_thunk (fun () -> raise (Failure "hi")) = None


(*-----------------------------------------------*)
let t5a = let f = fun () -> raise (Failure "")
          in try (try (thunk_of_pair (f, f)) with Failure "" -> (fun () -> (1, 1))) () =
                  (0, 0)
             with Failure "" -> true
                | _ -> false
let t5b = thunk_of_pair ((fun () -> 4), (fun () -> 5)) () = (4, 5)
let t5c = thunk_of_pair ((fun () -> "more"), (fun () -> "tests")) () = ("more", "tests")
let t5d = thunk_of_pair ((fun () -> [1; 2; 3]), (fun () -> [7; 8; 9])) () = ([1; 2; 3], [7; 8; 9])
let t5e = thunk_of_pair ((fun () -> Some 1), (fun () -> Some 2)) () = (Some 1, Some 2)
let t5f = thunk_of_pair ((fun () -> false), (fun () -> true)) () = (false, true)


(*-----------------------------------------------*)
let t6a = let f = fun () -> raise (Failure "")
          in try (try thunk_map (f, f)
                  with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false
let t6b = thunk_map ((fun () -> 4), (fun x -> 2 * x)) () = 8


(*-----------------------------------------------*)
let t7a = let f = fun () -> raise (Failure "")
          in try (try thunk_of_list [f; f]
                  with Failure "" -> (fun () -> [])) () = []
             with Failure "" -> true
                | _ -> false
let t7b = let f = fun () -> 5
          in thunk_of_list [f; f] () = [5; 5]







(*-----------------------------------------------*)
let t8a = insert (empty, "foo", 3) = [("foo", 3)]
let t8b = insert ([("foo", 3); ("bar", 2)], "stuff", 1) = [[("foo", 3); ("bar", 2); ("stuff", 1)] 
let t8c = insert ([("foo", 3); ("bar", 2)], "bar", 2) = [("foo", 3); ("bar", 2)] 
let t8d = insert () = [] 
let t8e = insert () = [] 
let t8f = insert () = [] 


(*-----------------------------------------------*)
let t9a = has ([("foo", 2)], "foo") = true
let t9b = has ([("First", 1); ("Second", 2)], "Second") = true
let t9c = has ([("Food", 4); ("Me", 3); ("Like", 2); ("Here", 1)], "Here") = true
let t9d = has ([("No", 2); ("None", 1)], "Yes") = false
let t9e = has ([("Yes", 3); ("Yep", 2); ("Yeah", 1)], "Nope") = false
let t9f = has ([], "nothing") = false


(*-----------------------------------------------*)
let t10a = lookup ([("bar", 3); ("foo", 2)], "bar") = 3
let t10b = try (lookup ([("bar", 3); ("foo", 2)], "baz"); false)
           with Not_found -> true
(* In the following test the search should fail because your code
   should stop looking after baz, since "baz" > "bar".
   This is of course not a "proper" table, but it is a good test that
   your code behaves properly. *)
let t10c = try (lookup ([("baz", 3); ("bar", 2)], "bar"); false)
           with Not_found -> true


(*-----------------------------------------------*)
let t11a = lookup_opt ([("bar", 3); ("foo", 2)], "bar") = Some 3
(* Again the search should be stopping after "foo" *)
let t11b = lookup_opt ([("foo", 2); ("bar", 3)], "bar") = None


(*-----------------------------------------------*)
let t12a = delete ([("bar", 3); ("baz", 1); ("foo", 2)], "bar") = [("baz", 1); ("foo", 2)]
let t12b = delete ([("First", 1); ("Second", 2); ("Third", 3)], "Third") = [("First", 1); ("Second", 2)]
let t12c = delete ([("First", 1); ("Second", 2)], "Third") = [("First", 1); ("Second", 2)]
let t12d = delete ([("First", 1); ("Second", 2); ("Third", 3)], "First") = [("Second", 2); ("Third", 3)]
let t12e = delete ([], "Nothing") = []
let t12f = delete ([("First", 1); ("Second", 2); ("Third", 3)], "Second") = [("First", 1); ("Third", 3)]


(*-----------------------------------------------*)
let t13a = keys [("bar", 3); ("foo", 2)] = ["bar"; "foo"]
let t13b = key [("first", 3)] = ["first"]
let t13c = key [("", )] = []
let t13d = key [("", )] = []
let t13e = key [("", )] = []
let t13f = key [("", )] = []

(*-----------------------------------------------*)
let t14a = is_proper [("bar", 3); ("foo", 2)] = true
