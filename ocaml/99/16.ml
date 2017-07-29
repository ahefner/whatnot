(* Drop every N'th element from a list. (medium)

# drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
 *)

let drop = fun list n ->
  let rec aux count = function
    | [] -> []
    | x :: t when (count==1) -> aux n t
    | x :: t -> x :: aux (count-1) t
  in aux n list ;;
