(* Compare the two methods of calculating Euler's totient function. (easy)

 Use the solutions of problems "Calculate Euler's totient function
 phi(m)" and "Calculate Euler's totient function phi(m) (improved)" to
 compare the algorithms.

# timeit phi 10090;;
- : float = 0.00574803352355957
# timeit phi_improved 10090;;
- : float = 6.98566436767578125e-05
 *)

let timeit f a =
  let start_time = Unix.gettimeofday() in
  let result = f a in
  let end_time = Unix.gettimeofday() in
  (end_time -. start_time, result) ;;
