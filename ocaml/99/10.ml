(* Run-length encoding of a list. (easy)
 Here is an example:

  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
  - : (int * string) list =
        [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
 *)

let rec encode = function
  | [] -> []
  | x :: t ->
     let rec aux = function (* ( count, string, t ),  : int * string * string list -> *)
       | ( n, x, [] ) -> [ (n,x) ]
       | ( n, x, y :: t) when x=y -> aux (n+1, x, t)
       | ( n, x, y :: t) -> (n,x) :: aux (1,y,t) ; in
     aux (1,x,t) ;;

  
