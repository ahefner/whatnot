type 'a node =
  | One of 'a
  | Many of 'a node list ;;

let rec flatten = function
  | One x -> [ x ]
  | Many [] -> []
  | Many (x :: t) -> List.append (flatten x) (flatten (Many t)) ;;

(* Close enough.. *)

  (*
 flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
  - : string list = ["a"; "b"; "c"; "d"; "e"]
   *)
