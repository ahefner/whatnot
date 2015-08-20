(* Group the elements of a set into disjoint subsets. (medium)

In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups.

# group ["a";"b";"c";"d"] [2;1];;
- : string list list list =
[[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]

(I don't think this problem description is clear, but refering to the
original 99 Prolog problems clarified it. In my opinion this problem
is mangled versus the original P-99 set, and the above example for
'group' is particulrly bad.)

 *)

let extend x = function
  | [] -> [[x]]
  | a::s -> (x::a)::s ;;

let rec aux = function
  | ([], _, _, _) -> []
  | ([0], acc, [], []) -> [acc]
  | (0::g, acc, t, rem) -> aux (g, []::acc, rem @ t, [])
  | (_,_,[],_) -> []
  | (n::g as ng, acc, x::t, rem) ->
     aux ((n-1)::g, (extend x acc), t, rem)
     @ aux (ng, acc, t, x::rem) ;;

let group items groups =
  aux (groups, [], items, []) ;;
