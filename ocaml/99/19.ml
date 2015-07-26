(* Rotate a list N places to the left. (medium)

 # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
  - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
                      # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
  - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
 *)

let rotate = fun list n ->
  let length = List.length list in
  if (length == 0) then [] else
    let n = (n mod length) in
    let rec aux i newtail = function
      | [] -> aux i newtail list
      | x::t when (i<n) -> aux (i+1) (x::newtail) t
      | t -> List.append t (List.rev newtail) in
    aux 0 [] list ;;

  (* Not great but it works *)
