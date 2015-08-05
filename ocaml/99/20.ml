(* Remove the K'th element from a list. (easy)

 The first element of the list is numbered 0, the second 1,...

 # remove_at 1 ["a";"b";"c";"d"];;
  - : string list = ["a"; "c"; "d"]
 *)

let rec remove_at n = function
  | [] -> []
  | x :: t when (n=0) -> t
  | x :: t -> x :: remove_at (n-1) t ;;

