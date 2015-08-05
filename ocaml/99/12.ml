(* Decode a run-length encoded list. (medium)

Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

Solution

# decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a ;;

let rec decode = function
  | [] -> []
  | One x :: t -> x :: decode t
  | Many (n,x) :: t when n > 0 -> x :: decode (Many (n-1,x) :: t)
  | Many (_,x) :: t -> decode t ;;
