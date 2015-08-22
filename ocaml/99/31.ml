(* Determine whether two positive integer numbers are coprime. (easy)
   Two numbers are coprime if their greatest common divisor equals 1.

 Solution

 # coprime 13 27;;
 - : bool = true
 # not (coprime 20536 7826);;
 - : bool = true
 *)

let coprime p q = (1 = gcd p q) ;;

