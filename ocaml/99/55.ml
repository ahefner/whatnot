(* Binary Trees *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let example_tree =
  Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
                Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty))) ;;

(* Construct completely balanced binary trees. (medium)

In a completely balanced binary tree, the following property holds for
every node: The number of nodes in its left subtree and the number of
nodes in its right subtree are almost equal, which means their
difference is not greater than one.

Write a function cbal_tree to construct completely balanced binary
trees for a given number of nodes. The function should generate all
solutions via backtracking. Put the letter 'x' as information into all
nodes of the tree.  *)

let is_even n = (n/2)*2 == n ;;

let cartesian_map f list_x list_y =
    List.fold_left
      (fun a_outer item_x ->
       List.fold_left (fun a_inner item_y -> f item_x item_y :: a_inner) a_outer list_y)
      []
      list_x ;;

let mknode l r = Node ('x',l,r) ;;

let rec cbal_tree = function
  | 0 -> [Empty]
  (*  | 1 -> [Node ('x',Empty,Empty)] *)
  | n -> if is_even (n-1) then
           let lprev = cbal_tree ((n-1)/2)
           in cartesian_map mknode lprev lprev
         else List.rev_append
                (cartesian_map mknode (cbal_tree ((n-1)/2)) (cbal_tree (1+(n-1)/2)))
                (cartesian_map mknode (cbal_tree (1+(n-1)/2)) (cbal_tree ((n-1)/2)))
;;





