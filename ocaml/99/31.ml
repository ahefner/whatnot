(* Determine whether a given integer number is prime. (medium)

# not(is_prime 1);;
- : bool = true
# is_prime 7;;
- : bool = true
# not (is_prime 12);;
- : bool = true
 *)

let is_prime n =
  let rec aux i =
    if (i <= 1) then true else if (n mod i == 0) then false else aux (i-1) in
  aux (n-1) ;;

