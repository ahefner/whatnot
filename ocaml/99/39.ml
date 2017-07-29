(* A list of prime numbers. (easy)

Given a range of integers by its lower and upper limit, construct a
list of all prime numbers in that range.

List.length (all_primes 2 7920);;
- : int = 1000

 *)

let all_primes min max =
  let rec aux n = if (n > max) then [] else if is_prime n then n::aux(n+1) else aux(n+1)
  in aux min ;;
