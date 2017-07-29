(* Find the last but one (last and penultimate) elements of a list. (easy) *)

let rec last_two = function
  | [] -> None
  | [x;y] -> Some x
  | _ :: t -> last_two t ;;

  
