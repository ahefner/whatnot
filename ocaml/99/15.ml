(* Replicate the elements of a list a given number of times. (medium)

# replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
 *)

let rec replicate = fun list count ->
  let rec aux = fun x n t ->
    if n >= 1 then x :: aux x (n-1) t else replicate t count in
  match list with
  | [] -> []
  | x :: t -> aux x count t ;;

  (* Not tail-recursive, the horror... *)

  

