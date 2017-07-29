(* Collect the internal nodes of a binary tree in a list. (easy)

An internal node of a binary tree has either one or two non-empty
successors. Write a function internals to collect them in a list. *)

let rec internals = function
  | Empty -> []
  | Node (_, Empty, Empty) -> []
  | Node (v, left, right) -> [ v ] @ (leaves left) @ (leaves right) ;;

(* Collect the nodes at a given level in a list. (easy)

A node of a binary tree is at level N if the path from the root to the
node has length N-1. The root node is at level 1. Write a function
at_level t l to collect all nodes of the tree t at level l in a
list.*)

let rec at_level n = function
  | Empty -> []
  | Node (x,l,r) -> if (n = 1) then [x]
                    else (at_level (n-1) l) @ (at_level (n-1) r) ;;
