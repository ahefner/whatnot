(* Layout a binary tree (1). (medium)

As a preparation for drawing the tree, a layout algorithm is required
to determine the position of each node in a rectangular grid. Several
layout methods are conceivable, one of them is shown in the
illustration.

Binary Tree Grid

In this layout strategy, the position of a node v is obtained by the
following two rules:

x(v) is equal to the position of the node v in the inorder sequence;
y(v) is equal to the depth of the node v in the tree.

In order to store the position of the nodes, we redefine the OCaml
type representing a node (and its successors) as follows: *)

type 'a pos_binary_tree =
  | E (* represents the empty tree *)
  | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree;;

  (* N(w,x,y,l,r) represents a (non-empty) binary tree with root w "positioned" at (x,y), and subtrees l and r. *)

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node('n',
       Node('k',
            Node('c',
                 leaf 'a',
                 Node('h',
                      Node('g',
                           leaf 'e',
                           Empty),
                      Empty)),
            leaf 'm'),
       Node('u',
            Node('p',
                 Empty,
                 Node('s',
                      leaf 'q',
                      Empty)),
            Empty));;

(* Write a function layout_binary_tree_1 with the following
specification: layout_binary_tree_1 t returns the "positioned" binary
tree obtained from the binary tree t. *)

let layout_binary_tree_1 tree =
  let rec layout xleft y = function
    | Empty -> (xleft,E)
    | Node (v,l,r) ->
       let (xc,lp) = layout xleft (y+1) l in
       let (xright,rp) = layout (xc+1) (y+1) r in
       (xright, N(v,xc,y,lp,rp))
  in let (_,tree) = layout 1 1 tree in tree ;;

