(* Goldbach's conjecture. (medium)

Goldbach's conjecture says that every positive even number greater
than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is
one of the most famous facts in number theory that has not been proved
to be correct in the general case. It has been numerically confirmed
up to very large numbers. Write a function to find the two prime
numbers that sum up to a given even integer.

# goldbach 28;;
- : int * int = (5, 23)
 *)

let goldbach n =
  let rec aux i = if (is_prime i) && (is_prime (n-i)) then (i,n-i) else aux (i+1)
  in aux 2 ;;
