(* Split a list into two parts; the length of the first part is given. (easy)

  If the length of the first part is longer than the entire list, then
  the first part is the list and the second part is empty.

 # split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
  - : string list * string list =
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
    # split ["a";"b";"c";"d"] 5;;
    - : string list * string list = (["a"; "b"; "c"; "d"], [])
 *)

let split = fun list n ->
  let rec aux = function
    | (_,a,[]) -> (List.rev a, [])
    | (i,a,b) when (i<=0) -> (List.rev a, b)
    | (i,a,x::t) -> aux ( (i-1), x :: a, t ) in
  aux (n, [], list) ;;
