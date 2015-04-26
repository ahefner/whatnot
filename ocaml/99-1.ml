
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t
;;

let x = 1 ;;

(* let () = assert (is_none (last [1])) ;;

let () = assert true ;;
*)

assert ((last [1]) = (Some 1));;

assert (last [] = None);;




