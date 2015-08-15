(* Group the elements of a set into disjoint subsets. (medium)

In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups.

# group ["a";"b";"c";"d"] [2;1];;
- : string list list list =
[[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]

 [ What?? Example output looks wrong. None of the above are exhaustive
   partitions of the four elements, there's always one missing.
   Maybed I've solved an entirely different problem. No matter, I like
   my problem better. ]
 *)

let group list groups =
  let rec aux accum = function
    (* Successful partitioning of the list. Return one result. *)
    | (m, _, 0, [], []) -> [accum::m]
    (* Picked one subgroup. Elements remaining, so recurse on this and
      next group spec. To avoid duplicate groups, recursing to build
      another instance of the current group size chooses only from the
      tail and not the already-skipped elements. The next group spec
      is allowed to choose from both the tail and the skipped
      elements. *)
    | (m, g::h::ght, n, t, skipped) when (n <= 0) ->
       aux [] (accum::m, g::h::ght, g, t, skipped) (* Same group *)
       @ aux [] (accum::m, h::ght, h, skipped @ t, []) (* Next group type *)
    (* Picked one subgroup, with elements remaining. Pick
      another. Same as above, but no additional group sizes, so no
      double recursion. *)
    | (m, [g], n, t, skipped) when (n <= 0) ->
       aux [] (accum::m, [g], g, t, skipped)
    (* Insufficient elements to choose this group *)
    | (_, _, n, [], _) (* when (n > 0) *) -> []
    (* Recurse, continuing to accumulate this group *)
    | (m, groups, n, x::t, skipped) ->
       aux (x::accum) (m, groups, n-1, t, skipped)
       @ aux accum (m, groups, n, t, x::skipped)
  in match groups with
     | [] -> []
     | g::t -> aux [] ([], g::t, g, list, [])
  ;;

