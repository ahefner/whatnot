(* Sorting a list of lists according to length of sublists. (medium)

We suppose that a list contains elements that are lists
themselves. The objective is to sort the elements of this list
according to their length. E.g. short lists first, longer lists later,
or vice versa.

# length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
- : string list list =
[["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 ["i"; "j"; "k"; "l"]]

*)

let length_sort = List.sort (fun x y -> compare (List.length x) (List.length y)) ;;

(* Again, we suppose that a list contains elements that are lists
themselves. But this time the objective is to sort the elements of
this list according to their length frequency; i.e., in the default,
where sorting is done ascendingly, lists with rare lengths are placed
first, others with a more frequent length come later.

# frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                   ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
- : string list list =
[["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 ["d"; "e"]; ["m"; "n"]]
*)

let frequency_sort list =
  let lengths = List.map List.length list in
  let count list x = List.fold_left (fun n y -> if (x=y) then n+1 else n) 0 list in
  List.sort (fun x y -> compare (count lengths (List.length x)) (count lengths (List.length y))) list ;;
