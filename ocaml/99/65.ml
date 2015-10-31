(* Layout a binary tree (2). (medium)

Binary Tree Grid

An alternative layout method is depicted in this illustration. Find
out the rules and write the corresponding OCaml function.

Hint: On a given level, the horizontal distance between neighbouring
nodes is constant. *)

let example_layout_tree_2 =
  let leaf x = Node (x,Empty,Empty) in
  Node('n', Node('k', Node('c', leaf 'a',
                           Node('e', leaf 'd', leaf 'g')),
                 leaf 'm'),
       Node('u', Node('p', Empty, leaf 'q'), Empty));;

let rec tree_height = function
  | Empty -> 0
  | Node (_,left,right) -> 1 + max (tree_height left) (tree_height right) ;;

let layout_binary_tree_2 tree =
  let height = tree_height tree in
  let rec aux index depth = function
    | Empty -> E
    | Node (v,l,r) -> N (v,
                         (2*index+1) * pow 2 (height-depth-1),
                         1+depth,
                         aux (index*2) (depth+1) l,
                         aux (index*2+1) (depth+1) r)
  in
  let rec find_min_x = function
    | E -> 0
    | N (_,x,_, E,_) -> x
    | N (_,_,_, (N _ as left), _) -> find_min_x left
  in
  let rec adjust offset = function
    | E -> E
    | N (v,x,y,l,r) -> N (v, x+offset, y, adjust offset l, adjust offset r)
  in
  let layout = aux 0 0 tree
  in adjust (1 - find_min_x layout) layout ;;
