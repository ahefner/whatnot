(* Count the leaves of a binary tree. (easy)

A leaf is a node with no successors. Write a function count_leaves to
count them. *)

let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, left, right) -> count_leaves left + count_leaves right ;;

(* Collect the leaves of a binary tree in a list. (easy)

A leaf is a node with no successors. Write a function leaves to
collect them in a list.*)

let rec leaves = function
  | Empty -> []
  | Node (value, Empty, Empty) -> [ value ]
  | Node (_, left, right) -> leaves left @ leaves right;;

