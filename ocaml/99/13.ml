(* Run-length encoding of a list (direct solution). (medium)

 Implement the so-called run-length encoding data compression method
 directly. I.e. don't explicitly create the sublists containing the
 duplicates, as in problem "Pack consecutive duplicates of list
 elements into sublists", but only count them. As in problem "Modified
 run-length encoding", simplify the result list by replacing the
 singleton lists (1 X) by X.

 # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
  - : string rle list =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
    Many (4, "e")]
 *)

let rec encode = function
  | [] -> []
  | x :: t ->
     let rec aux count = function
       | y :: s when (x = y) -> aux (count+1) s
       | t when (count = 1) -> One x :: encode t
       | t -> Many (count,x) :: encode t ; in
     aux 1 t ;;
