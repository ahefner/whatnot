(* Calculate Euler's totient function (medium)

Euler's so-called totient function is defined as the number of
positive integers r (1 <= r < m) that are coprime to m. We let phi(1)
= 1.

# phi 10;;
- : int = 4
# phi 13;;
- : int = 12
 *)

let phi r =
  let rec aux i a = if (i <= 1) then a+1 else aux (i-1) (a + if (coprime i r) then 1 else 0) in
  aux (r-1) 0 ;;
