(* A list of Goldbach compositions. (medium)

Given a range of integers by its lower and upper limit, print a list
of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime
numbers, one of them is very small. Very rarely, the primes are both
bigger than say 50. Try to find out how many such cases there are in
the range 2..3000.

# goldbach_list 9 20;;
- : (int * (int * int)) list =
[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 (20, (3, 17))]
# goldbach_limit 1 2000 50;;
- : (int * (int * int)) list =
[(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
 (1928, (61, 1867))]

 *)

let goldbach_list min max =
  let rec aux i = if (i <= max/2) then ((i*2),goldbach (i*2))::aux(i+1) else []
  in aux ((min+1)/2) ;;

let goldbach_limit min max threshold =
  List.filter (function
                | (_,(p,q)) when (p>threshold) && (q>threshold) -> true
                | _ -> false)
              (goldbach_list min max) ;;

