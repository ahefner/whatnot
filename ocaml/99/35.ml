(* Determine the prime factors of a given positive integer. (medium)

Construct a flat list containing the prime factors in ascending order.

# factors 315;;
- : int list = [3; 3; 5; 7]
 *)

let rec factors n =
  let rec factorize n i =
    if (i >= n) then [n]
    else if ((n mod i) == 0)
    then i :: factorize (n/i) 2
    else factorize n (i+1)
  in factorize n 2 ;;

