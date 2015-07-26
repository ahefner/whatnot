(* Duplicate the elements of a list. (easy)

# duplicate ["a";"b";"c";"c";"d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
 *)

let rec duplicate = function
  | [] -> []
  | x :: t -> x :: x :: duplicate t ;;

  
