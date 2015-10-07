(* Determine the greatest common divisor of two positive integer numbers. (medium)

Use Euclid's algorithm.

# gcd 13 27;;
- : int = 1
# gcd 20536 7826;;
- : int = 2
 *)

let rec gcd a b =
  let x = min a b in
  let y = max a b in
  if (y mod x == 0) then x else gcd x (y mod x) ;;

