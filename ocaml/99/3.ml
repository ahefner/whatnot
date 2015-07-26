(* Find the k'th element of a list. (easy) *)

let rec at k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k-1) t ;;
  
