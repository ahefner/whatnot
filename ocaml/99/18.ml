(* Extract a slice from a list. (medium)

Given two indices, i and k, the slice is the list containing the
elements between the i'th and k'th element of the original list (both
limits included). Start counting the elements with 0 (this is the way
the List module numbers elements).

# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"]
*)

let slice = fun list start endidx ->
  let rec aux n accum = function
    | [] -> List.rev accum
    | _ :: t when (n < start) -> aux (n+1) [] t
    | _ :: t when (n > endidx) -> List.rev accum
    | x :: t -> aux (n+1) (x::accum) t in
  aux 0 [] list ;;
