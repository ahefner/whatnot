(* Binary search trees (dictionaries). (medium)

Construct a binary search tree from a list of integer numbers. *)

let rec construct = function
  | [] -> Empty
  | x :: t ->
     Node (x, construct (List.filter ((>) x) t), construct (List.filter ((<) x) t)) ;;

