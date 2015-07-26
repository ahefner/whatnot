(* Insert an element at a given position into a list. (easy)

 Start counting list elements with 0. If the position is larger or
 equal to the length of the list, insert the element at the end. (The
 behavior is unspecified if the position is negative.)

 # insert_at "alfa" 1 ["a";"b";"c";"d"];;
  - : string list = ["a"; "alfa"; "b"; "c"; "d"]
 # insert_at "alfa" 3 ["a";"b";"c";"d"];;
  - : string list = ["a"; "b"; "c"; "alfa"; "d"]
 # insert_at "alfa" 4 ["a";"b";"c";"d"];;
  - : string list = ["a"; "b"; "c"; "d"; "alfa"]
 *)

let rec insert_at elt index = function
  | t when (index == 0) -> elt :: t
  | [] -> []
  | x :: t -> x :: insert_at elt (index-1) t ;;

