(* Symmetric binary trees. (medium)

Let us call a binary tree symmetric if you can draw a vertical line
through the root node and then the right subtree is the mirror image
of the left subtree. Write a function is_symmetric to check whether a
given binary tree is symmetric.

Hint: Write a function is_mirror first to check whether one tree is
the mirror image of another. We are only interested in the structure,
not in the contents of the nodes.
 *)

let rec is_mirror = function
  | (Empty,Empty) -> true
  | (Node (_,ll,lr), Node (_,rl,rr)) -> is_mirror (ll,rr) && is_mirror (lr,rl)
  | _ -> false ;;

let is_symmetric = function
  | Empty -> true
  | Node (_,l,r) -> is_mirror (l,r) ;;

