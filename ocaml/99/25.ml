(* Generate a random permutation of the elements of a list. (easy)

# permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
- : string list = ["a"; "e"; "f"; "b"; "d"; "c"]  *)

exception InvalidArg of string ;;

(* Non-empty complete binary tree, with lists of integers in the leaves *)
type 'a ctree =
  | Seg of int * 'a list
  | Subtree of int * 'a ctree * 'a ctree ;;

type 'a tree =
  | Tree of 'a ctree
  | EmptyTree ;;

let rec nthtail n = function
  | [] -> raise (InvalidArg "List can't be empty!")
  | x :: t when (n<=0) -> x,t
  | _ :: t -> nthtail (n-1) t ;;

let ctreeLen = function
  | Subtree (len,_,_) -> len
  | Seg (len,_) -> len ;;

let makeSeg n t = if (n<=0) then EmptyTree else Tree (Seg (n,t)) ;;
let makeSubtree = function
  | (EmptyTree,EmptyTree) -> EmptyTree
  | (EmptyTree,x) | (x,EmptyTree) -> x
  | (Tree x, Tree y) -> Tree (Subtree ((ctreeLen x) + (ctreeLen y), x, y)) ;;

let selSeg index = function
  | (n,x::t) when (index==0) -> x, makeSeg (n-1) t
  | (n,t) -> let x,u = (nthtail index t) in x, makeSubtree ((makeSeg index t), makeSeg (n-index-1) u) ;;

let rec select index = function
  | Subtree (n,l,r) ->
     assert (index < n) ;
     let lenl = ctreeLen l in
     if (index < lenl)
     then let (x,nl) = (select index l) in (x, makeSubtree (nl, Tree r))
     else let (x,nr) = (select (index-lenl) r) in (x, makeSubtree (Tree l, nr))
  | Seg (n,t) -> selSeg index (n,t)
;;

let makeTree (type a) = function
  | [] -> EmptyTree
  | x -> Tree (Seg ((List.length x), x)) ;;

let permutation list =
  let rec aux = function
    | EmptyTree -> []
    | Tree s -> let (x,t) = select (Random.int (ctreeLen s)) s in x :: aux t
      in aux (makeTree list) ;;
