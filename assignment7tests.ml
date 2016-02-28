

let t1a = range1 (5) = [1; 2; 3; 4; 5]
let t1b = range1 (3) = [1; 2; 3]
let t1c = range1 (4) = [1; 2; 3; 4]
let t1d = range1 (7) = [1; 2; 3; 4; 5; 6; 7]

(*---------------*)

let t2a = tabulate ((fun x -> x + x) 4) = [2; 4; 6; 8]
let t2b = tabulate ((fun x -> x - 1) 4) = [0; 1; 2; 3]
let t2c = tabulate ((fun x -> x * x) 5) = [1; 4; 9; 16; 25]
let t2d = tabulate ((fun x -> x + 5) 5) = [6; 7; 8; 9; 10]

(*---------------*)

let t3a = string_of_pix (D) = "."
let t3b = string_of_pix (H) = "#"
let t3c = string_of_pix (D) = "."
let t3d = string_of_pix (H) = "#"

(*--------------*)

let t4a = string_of_row ([D; D; D; D]) = "...."
let t4b = string_of_row ([D; H; D: D;]) = ".#.."
let t4c = string_of_row ([H; H; H; H;]) = "####"
let t4d = string_of_row (D; H; D; H) = ".#.#"
let t4e = string_of_row (H; H; D; D; H) = "##..#"

(*-----------JUST IN CASE-------*)


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


(*---------------------*)

let t5a = string_of_pic (doodad) =
"
.....#.....
....##.....
...#.#.....
.....#.....
.....#.....
.....#.....
.....#.....
.....#.....
...#####...
"


let t5b = string_of_pic (sword) =
"
................
.##.............
.####...........
..####..........
..#####.........
...####.........
....####........
......###.......
.......###..#...
........##.##...
..........##....
.........###....
.........##..#..
.............#..
..............##
..............##
"

(*----------------*)

let t6a = flip_vertical (doodad) =
"
.....#.....
.....##....
.....#.#...
.....#.....
.....#.....
.....#.....
.....#.....
.....#.....
...#####...
"



let t6b = flip_horizontal (doodad) = 
"
...#####...
.....#.....
.....#.....
.....#.....
.....#.....
.....#.....
.....#.#...
.....##....
.....#.....
"


let t6c = flip_both (doodad) =
"
...#####...
.....#.....
.....#.....
.....#.....
.....#.....
.....#.....
...#.#.....
....##.....
.....#.....
"


(*-----------------*)


let t7a = mirror_vertical (doodad) =
[[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; H; H; D; D; D; D; D]
[D; D; D; H; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; H; H; H; H; H; D; D; D]]
[D; D; D; H; H; H; H; H; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]
[D; D; D; H; D; H; D; D; D; D; D]
[D; D; D; D; H; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]]



let t7b = mirror_horizontal (doodad) =
[[D; D; D; D; D; H; D; D; D; D; D][[D; D; D; D; D; H; D; D; D; D; D;]
[D; D; D; D; H; H; D; D; D; D; D] [D; D; D; D; D; H; H; D; D; D; D; D]
[D; D; D; H; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; H; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; H; H; H; H; H; D; D; D]][D; D; D; H; H; H; H; H; D; D; D; D]



let t7c = mirror_both (doodad) =

[[D; D; D; D; D; H; D; D; D; D; D][[D; D; D; D; D; H; D; D; D; D; D;]
[D; D; D; D; H; H; D; D; D; D; D] [D; D; D; D; D; H; H; D; D; D; D; D]
[D; D; D; H; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; H; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; H; H; H; H; H; D; D; D]][D; D; D; H; H; H; H; H; D; D; D; D]
[D; D; D; H; H; H; H; H; D; D; D] [D; D; D; H; H; H; H; H; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; D; D; D; D; D]
[D; D; D; H; D; H; D; D; D; D; D] [D; D; D; D; D; H; D; H; D; D; D; D]
[D; D; D; D; H; H; D; D; D; D; D] [D; D; D; D; D; H; H; D; D; D; D; D]
[D; D; D; D; D; H; D; D; D; D; D]][D; D; D; D; D; H; D; D; D; D; D; D]

(*-------------------------*)

