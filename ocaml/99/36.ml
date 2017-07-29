(* Determine the prime factors of a given positive integer (2). (medium)

Construct a list containing the prime factors and their
multiplicity. Hint: The problem is similar to problem Run-length
encoding of a list (direct solution).

# factors 315;;
- : (int * int) list = [(3, 2); (5, 1); (7, 1)]
 *)

let rle = function
  | [] -> []
  | x::t ->
     let rec aux = function
       | (t, []) -> [t]
       | ((p,n), q::t) when (p=q) -> aux ((p,n+1),t)
       | (prev, q::t) -> prev :: aux((q,1),t)
     in aux ((x,1),t) ;;

let factors2 list = rle (factors list);;


