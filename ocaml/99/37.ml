(* Calcaulte Euler's totient function phi(m) (improved)

If the list of the prime factors of a number m is known in the form of the previous problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:

phi(m) = (p1 - 1) * p1^(m1-1) * (p2 - 1)^(m2-1)*...

# phi_improved 10;;
- : int = 4
# phi_improved 13;;
- : int = 12

 *)

let rec pow x = function
  | 0 -> 1
  | 1 -> x
  | n when (n mod 2 == 0) -> pow (x*x) (n/2)
  | n -> x * pow x (n-1) ;;

let phi_improved n = List.fold_left (fun x (p,m) -> x * (p-1) * pow (p) (m-1)) 1 (factors2 n) ;;
