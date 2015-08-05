(* Pack consecutive duplicates of list elements into sublists. (medium)

# pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
 *)

let rec pack = function
  | [] -> []
  | x :: t ->
     let rec aux accum = function
       | [] -> [ accum ]
       | x :: t when (x = List.hd accum) -> aux (x :: accum) t
       | x :: t -> accum :: aux [x] t ; in
     aux [x] t ;;
  
