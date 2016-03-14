(* Programming Languages, Assignment 7 *)
(*
   You should write your functions in this file.
   You should NOT specify the types of your functions. Let the system determine
   them for you.

   The instructions for this assignment reside in an auxiliary file, assignment7doc.md
   You should start by reading that file.
*)
(* ---------------------------------
              HELPERS
   ---------------------------------

   Place your "helpers" implementations here.
*)
let rec range a b = 
   if a > b 
   then [] 
   else a :: range (a + 1) b


let range1 (n) =
   range 1 n


let tabulate (f, n) =
   List.fold_right (fun x rest -> (f x) :: rest) range (1 n) []


(* ---------------------------------
              PICTURES
   ---------------------------------

   Place our Pictures implementations here after the type declarations and
   sword definition.
*)
type pixel = D | H
type row = pixel list
type pic = row list

exception IncompatibleDims

let sword = [
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;H;H;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;H;H;H;D;D;D;D;D;D;D;D;D];
[D;D;D;H;H;H;H;D;D;D;D;D;D;D;D;D];
[D;D;D;D;H;H;H;H;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;H;H;H;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;H;H;H;D;D;H;D;D;D];
[D;D;D;D;D;D;D;D;H;H;D;H;H;D;D;D];
[D;D;D;D;D;D;D;D;D;D;H;H;D;D;D;D];
[D;D;D;D;D;D;D;D;D;H;H;H;D;D;D;D];
[D;D;D;D;D;D;D;D;H;H;D;D;H;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;H;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H]]


let doodad = [
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; H; H; D; D; D; D; D]
[D; D; D; H; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; H; H; H; H; H; D; D; D]]

(*
   These two functions provided to you. Study how they work before continuing!
*)
let valid_pic pic =
   match List.map List.length pic with
   | [] -> true
   | x :: xs -> List.for_all ((=) x) xs

let dims_pic pic =
   match pic with
   | [] -> (0, 0)
   | row :: _ -> (List.length pic, List.length row)


(*-------- STRING_OF Functions -------*)

let string_of_pxl (pxl) = 
   match pxl with
   | D -> "."
   | H -> "#"


let string_of_row (pxl_lst) =
   List.fold_right (fun x acc -> 
      if x = D
      then "." ^ acc
      else "#" ^ acc)
                     pxl_lst "\n"



let string_of_pic (pic) =
   List.fold_right (fun x acc ->
      string_of_row x @^ acc)


(*---------- FLIP Functions ----------*)

let flip_vertical (pic) =
   List.fold_left (fun x acc ->
                  string_of_row x ^ acc)
                  pic []

let flip_horizontal (pic) =
   List.fold_left (fun x acc ->
                  )


let flip_both (pic) =
   flip_horizontal (flip_vertical (pic))


(*-------------------------------------*)

let mirror_vertical (pic) =
   string_of_pic (pic) @ flip_vertical (pic)


let mirror_horizontal (pic) =
   string_of_pic (pic) @ flip_horizontal (pic)

let mirror_both (pic) = 
   string_of_pic (pic) @ mirror_horizontal (pic) @ mirror_vertical (pic) @ (mirror_horizontal (mirror_vertical (pic))) 


(*-------------------------------------)

let pixelate ()

(------------------------------------*)


let stack_vertical (pic1, pic2) = 
   if String.length (string_of_pic (pic1)) != String.length (string_of_pic (pic2))
   then raise IncompatibleDims
   else pic1 ^ "\n" ^ pic2 



let stack_horizontal (pic1, pic2) = 
   if String.length (string_of_pic (pic1)) != String.length (string_of_pic (pic2))
   then raise IncompatibleDims
   else pic1 ^ pic2 


let invert (pic) =
