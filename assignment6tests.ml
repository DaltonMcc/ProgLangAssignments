(* These tests assume that you have implemented the function `take` as suggested *)

(*-------------------------------------------------------------*)
let t1a = take 4 (const 3) = [3; 3; 3; 3]
let t1b = take 3 (const 5) = [5; 5; 5]
let t1c = take 4 (const 1) = [1; 1; 1; 1]
let t1d = take 3 (const 9) = [9; 9; 9]
let t1e = take 4 (const 4) = [4; 4; 4; 4]

(*-------------------------------------------------------------*)
let t2a = take 5 (alt 3 4) = [3; 4; 3; 4; 3]
let t2b = take 5 (alt 1 2) = [1; 2; 1; 2; 1]
let t2c = take 4 (alt 4 2) = [4; 2; 4; 2]
let t2d = take 6 (alt 3 1) = [3; 1; 3 ;1 ;3 ;1]
let t2e = take 5 (alt 1 9) = [1; 9; 1; 9; 1]
let t2f = take 2 (alt 8 7) = [8; 7]


(*-------------------------------------------------------------*)
let t3a = take 3 (seq 2 6) = [2; 8; 14]
let t3b = take 4 (seq 2 10) = [2; 12; 22; 32]
let t3c = take 4 (seq 3 3) = [3; 6; 9; 12]
let t3d = take 3 (seq 5 1) = [5; 6; 7]
let t3e = take 5 (seq 2 2) = [2; 4; 6; 8; 10]
let t3f = take 4 (seq 1 2) = [1; 3; 5; 7]


(*-------------------------------------------------------------*)
let t4a = take 5 (from_f (fun x -> x * x)) = [1; 4; 9; 16; 25]
(* The next test ensures that the function is not called until the corresponding
   value is actually needed. *)
let t4b = try (ignore (from_f (fun _ -> raise (Failure ""))); true) with
          | _ -> false
let t4c = take 4 (from_f (fun x -> x + x)) = [2; 4; 6; 8]
let t4d = take 3 (from_f (fun x -> x - x)) = [0; 0; 0]
let t4e = take 4 (from_f (fun x -> x + 2)) = [3; 4; 5; 6]
let t4f = take 5 (from_f (fun x -> x * 2)) = [1; 4; 6; 8; 10]
let t4g = take 4 (from_f (fun x -> (x + 1) + x) = [3; 5; 7; 9]



(*-------------------------------------------------------------*)
let t5a = take 5 (from_list [3; 5; 6]) = [3; 5; 6; 3; 5]
let t5b = take 6 (from_list [1; 2; 3]) = [1; 2; 3; 1; 2; 3]
let t5c = take 4 (from_list [4; 5]) = [4; 5; 4; 5]
let t5d = take 5 (from_list [3; 2; 1]) = [3; 2; 1; 3; 2]
let t5e = take 6 (from_list [9; 6]) = [9; 6; 9; 6; 9; 6]
let t5f = take 6 (from_list [3; 6; 8]) = [3; 6; 8; 3; 6; 8]


(*-------------------------------------------------------------*)
let t6a = take 3 (drop 3 (seq 2 6)) = [20; 26; 32]
let t6b = take 4 (drop 2 (seq 5 1)) = [7; 8; 9; 10]
let t6c = take 4 (drop 1 (seq 1 2)) = [3; 5; 7; 9]
let t6d = take 3 (drop 3 (alt 1 5)) = [5; 1; 5]
let t6e = take 4 (drop 4 (seq 1 1)) = [5; 6; 7; 8]
let t6f = take 4 (drop 3 (seq 3 2)) = [9; 12; 15; 18]


(*-------------------------------------------------------------*)
let t7a = take 6 (prepend [1; 2] (const 3)) = [1; 2; 3; 3; 3; 3]
let t7b = take 6 (prepend [1; 1; 1] (alt 3 4)) = [1; 1; 1; 3; 4; 3]
let t7c = take 8 (prepend [1; 2; 3] (from_list [4; 5; 6])) = [1; 2; 3; 4; 5; 6; 4; 5]
let t7d = take 6 (prepend [2; 2] (alt 1 2)) = [2; 2; 1; 2; 1; 2]
let t7e = take 5 (prepend [3; 2] (seq 1 1)) = [3; 2; 1; 2; 3]
let t7f = take 6 (prepend [1; 0; 0; 1] (const 0)) = [1; 0; 0; 1; 0 ; 0]


(*-------------------------------------------------------------*)
let t8a = take 6 (map (fun x -> x * x) (seq 1 1)) = [1; 4; 9; 16; 25; 36]
(* The next test ensures that the function is not called until the corresponding
   value is actually needed. *)
let t8b = try (ignore (map (fun _ -> raise (Failure "")) (seq 1 1)); true) with
          | _ -> false


(*-------------------------------------------------------------*)
let t9a = take 3 (pair_up (seq 1 1)) = [(1, 2); (3, 4); (5, 6)]
let t9b = take 4 (pair_up (const 2)) = [(2, 2); (2, 2); (2, 2); (2, 2)]
let t9c = take 3 (pair_up (seq 1 2)) = [(1, 3); (5, 7); (9, 11)]
let t9d = take 3 (pair_up (seq 3 3)) = [(3, 6); (9, 12); (15, 18)]
let t9e = take 4 (pair_up (const 6)) = [(6, 6); (6, 6); (6, 6); (6, 6)]
let t9f = take 5 (pair_up (alt 5 7)) = [(5, 7); (5, 7); (5, 7); (5, 7); (5, 7)]


(*-------------------------------------------------------------*)
let t10a = take 3 (zip2 (seq 1 2) (seq 2 3)) = [(1, 2); (3, 5); (5, 8)]
let t10b = take 4 (zip2 (alt 1 2) (alt 3 4)) = [(1, 3); (2, 4); (1, 3); (2, 4)]
let t10c = take 3 (zip2 (seq 1 1) (alt 2 3)) = [(1, 2); (2, 3); (3, 2)]
let t10d = take 4 (zip2 (alt 0 9) (alt 4 8)) = [(0, 4); (9, 8); (0, 4); (9, 8)]
let t10e = take 4 (zip2 (seq 1 2) (alt 5 0)) = [(1, 5); (3, 0); (5, 5); (7, 0)]
let t10f = take 3 (zip2 (alt 3 6) (const 2)) = [(3, 2); (6, 2); (3, 2)]


(*-------------------------------------------------------------*)
let t11a = take 4 (accum (+) 0 (seq 1 1)) = [0; 1; 3; 6]
let t11b = take (accum (-) 1 ()) = []
let t11c = take (accum (-) 5 ()) = []
let t11d = take (accum (+) 5 ()) = []
let t11e = take (accum (+) 2 ()) = []
let t11f = take (accum (-) 2 ()) = []


(*-------------------------------------------------------------*)
let t12a = take 4 (filter (fun x -> x mod 2 = 0) (seq 1 1)) = [2; 4; 6; 8]
let t12b = take (filter () ()) = []
let t12c = take (filter () ()) = []
let t12d = take (filter () ()) = []
let t12e = take (filter () ()) = []
let t12f = take (filter () ()) = []


(*-------------------------------------------------------------*)
let t13a = take 3 (collect 3 (seq 1 1)) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]
let t13b = take (collect ()) = []
let t13c = take (collect ()) = []
let t13d = take (collect ()) = []
let t13e = take (collect ()) = []
let t13f = take (collect ()) = []


(*-------------------------------------------------------------*)
let t14a = take 5 (flatten (collect 3 (seq 1 1))) = [1; 2; 3; 4; 5]
let t14b = take (flatten ()) = []
let t14c = take (flatten ()) = []
let t14d = take (flatten ()) = []
let t14e = take (flatten ()) = []
let t14f = take (flatten ()) = []


(*-------------------------------------------------------------*)
let t15a = take 4 (list_combos (seq 1 1) (seq 1 1)) =
                  [[(1, 1)]; [(2, 1); (1, 2)]; [(3, 1); (2, 2); (1, 3)];
                   [(4, 1); (3, 2); (2, 3); (1, 4)]]


(*-------------------------------------------------------------*)
let t16a = take 10 (list_combos_flat (seq 1 1) (seq 1 1)) =
                  [(1, 1); (2, 1); (1, 2); (3, 1); (2, 2); (1, 3);
                   (4, 1); (3, 2); (2, 3); (1, 4)]


(*
   The following test is an integration test putting together some of the things
   you've build so far. It builds a stream that produces (albeit not terribly
   efficiently) all pythagorean triples:
   https://en.wikipedia.org/wiki/Pythagorean_triple
   It does the following:
   - Starts with the sequence of natural numbers 1, 2, 3, 4, using `seq`
   - Forms all possible pairs of pairs ((a, b), c) from that sequence using
   list_combos_flat
   - Filters that list using the condition a^2 + b^2 = c^2 and the `filter` function

   Then we read out the first 5 answers.
*)
let t17a = let nats = seq 1 1 in
           let pairs = list_combos_flat (list_combos_flat nats nats) nats in
           let triples = filter (fun ((a, b), c) -> a * a + b * b = c * c) pairs
           in take 8 triples = [((4, 3), 5); ((3, 4), 5); ((8, 6), 10); ((6, 8), 10);
                                ((12, 5), 13); ((5, 12), 13); ((12, 9), 15); ((9, 12), 15)]

(*
   This integration tests shows that whenever you add all consecutive integers starting
   from 1 then the result is all the perfect squares:
*)
let t17b = let odds = seq 1 2 in
           let sums = accum (+) 0 odds in
           let squares = map (fun x -> x * x) (seq 0 1)
           in take 8 (zip2 sums squares) =
                  [(0, 0); (1, 1); (4, 4); (9, 9);
                   (16, 16); (25, 25); (36, 36); (49, 49)]
