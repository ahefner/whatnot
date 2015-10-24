(* Construct height-balanced binary trees with a given number of nodes. (medium)

Consider a height-balanced binary tree of height h. What is the
maximum number of nodes it can contain? Clearly, maxN = 2^h -
1. However, what is the minimum number minN? This question is more
difficult. Try to find a recursive statement and turn it into a
function min_nodes defined as follows: min_nodes h returns the minimum
number of nodes in a height-balanced binary tree of height h.
 *)

let rec min_nodes = function
  | 0 -> 0
  | 1 -> 1
  | n -> 1 + min_nodes (n-1) + min_nodes (n-2) ;;

(* On the other hand, we might ask: what is the maximum height H a
height-balanced binary tree with N nodes can have? max_height n
returns the maximum height of a height-balanced binary tree with n
nodes. *)

let max_height n =
  let rec aux h p q =
    if n < (1+p+q) then h
    else aux (h+1) (1+p+q) p
  in aux 0 0 0 ;;

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
     let b = pow a (n / 2) in
         b * b * (if n mod 2 = 0 then 1 else a)

let max_nodes h = (pow 2 h) - 1 ;;

let min_height_nodes n =
  let rec aux h =
    if (n >= min_nodes h) && (n <= max_nodes h) then h else aux (h+1)
  in aux 0 ;;

let min_max_height n =
  let rec aux h p q minh =
    if n < (1+p+q) then (minh,h)
    else aux (h+1) (1+p+q) p (if (n <= (max_nodes h)) && (n >= p) then minh else h+1)
  in aux 0 0 0 0 ;;

let rec count_nodes = function
  | Empty -> 0
  | Node (_,left,right) -> 1 + count_nodes left + count_nodes right ;;

let hbal_tree_nodes n =
  let (minh,maxh) = min_max_height n in
  let rec aux h =
    if h >= minh then (hbal_tree h) @ (aux (h-1)) else []
  in List.filter (fun t -> n == count_nodes t) (aux maxh) ;;
