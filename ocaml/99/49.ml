(* Gray code. (medium)

 An n-bit Gray code is a sequence of n-bit strings constructed
 according to certain rules. For example,

 n = 1: C(1) = ['0','1'].
 n = 2: C(2) = ['00','01','11','10'].
 n = 3: C(3) = ['000','001',',,,,,,,,,,,,].

Find out the construction rules and write a function with the
following specification: gray n returns the n-bit Gray code.
 *)

let rec gray = function
  | n when n <= 0 -> []
  | 1 -> ["0";"1"]
  | n -> let p = gray (n-1) in
         (List.map ((^) "0") p) @ (List.map ((^) "1") (List.rev p)) ;;

