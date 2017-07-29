(* Extract a given number of randomly selected elements from a list. (medium)

 The selected items shall be returned in a list. We use the Random
 module but do not initialize it with Random.self_init for
 reproducibility.

 # rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
 - : string list = ["g"; "d"; "a"]
 *)

let rand_select list n =
  let rec aux len n = function
    | [] -> []
    | _ when (n <= 0) -> []
    | x :: t when (Random.int len < n) -> x :: aux (len-1) (n-1) t
    | _ :: t -> aux (len-1) n t in
  aux (List.length list) n list ;;


