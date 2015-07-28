(* Create a list containing all integers within a given range. (easy)

 If first argument is smaller than second, produce a list in decreasing order.

 # range 4 9;;
 - : int list = [4; 5; 6; 7; 8; 9]
 # range 9 4;;
 - : int list = [9; 8; 7; 6; 5; 4]
 *)

let rec range i j =
  if (i < j) then i :: range (i+1) j
  else if (i > j) then i :: range (i-1) j
  else [j] ;;
