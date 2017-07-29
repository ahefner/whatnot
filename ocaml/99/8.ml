(* Eliminate consecutive duplicates of list elements. (medium)

 # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
  - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
 *)

let rec compress = function
  | [] -> []
  | x :: y :: t when x = y -> seek (x :: t)
  | x :: t -> x :: seek t

