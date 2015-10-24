(* Construct height-balanced binary trees. (medium)

In a height-balanced binary tree, the following property holds for
every node: The height of its left subtree and the height of its right
subtree are almost equal, which means their difference is not greater
than one.

Write a function hbal_tree to construct height-balanced binary trees
for a given height. The function should generate all solutions via
backtracking. Put the letter 'x' as information into all nodes of the
tree. *)

let rec hbal_tree = function
  | 0 -> [ Empty ]
  | 1 -> [ Node ('x',Empty,Empty) ]
  | n ->
     let sub1 = hbal_tree (n-1) in
     let sub2 = hbal_tree (n-2) in
     (cartesian_map mknode sub1 sub1)
     @ (cartesian_map mknode sub1 sub2)
     @ (cartesian_map mknode sub2 sub1) ;;
