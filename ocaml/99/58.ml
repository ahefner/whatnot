(* Generate-and-test paradigm. (medium)

Apply the generate-and-test paradigm to construct all symmetric,
completely balanced binary trees with a given number of nodes.
 *)

let sym_cbal_trees n = List.filter is_symmetric (cbal_tree n) ;;
