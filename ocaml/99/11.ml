(* Modified run-length encoding. (easy)

 Modify the result of the previous problem in such a way that if an
 element has no duplicates it is simply copied into the result
 list. Only elements with duplicates are transferred as (N E) lists.

 Since OCaml lists are homogeneous, one needs to define a type to hold
 both single elements and sub-lists.

 # type 'a rle =
    | One of 'a
    | Many of int * 'a;;

   type 'a rle = One of 'a | Many of int * 'a
                                                                                                                                          # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 - : string rle list =
 [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a ;;

let rec encode = function
  | [] -> []
  | x :: t ->
     let rec aux = function
       | ( z, [] ) -> [ z ]
       | ( One x, y :: t ) when x = y -> aux ( Many (2,x), t )
       | ( Many (n,x), y :: t ) when x = y -> aux ( Many (n+1,x), t )
       | ( One x, y :: t ) -> One x :: aux (One y, t)
       | ( Many (n,x), y :: t) -> Many (n,y) :: aux (One y, t) ; in
     aux (One x, t) ;;

