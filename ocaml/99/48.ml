(* Truth tables for logical expressions. (medium)

Generalize the previous problem in such a way that the logical
expression may contain any number of logical variables. Define table
in a way that table variables expr returns the truth table for the
expression expr, which contains the logical variables enumerated in
variables.

# table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")));;
- : ((string * bool) list * bool) list =
[([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
 ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
# let a = Var "a" and b = Var "b" and c = Var "c" in
  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;
- : ((string * bool) list * bool) list =
[([("a", true); ("b", true); ("c", true)], true);
 ([("a", true); ("b", true); ("c", false)], true);
 ([("a", true); ("b", false); ("c", true)], true);
 ([("a", true); ("b", false); ("c", false)], false);
 ([("a", false); ("b", true); ("c", true)], false);
 ([("a", false); ("b", true); ("c", false)], false);
 ([("a", false); ("b", false); ("c", true)], false);
 ([("a", false); ("b", false); ("c", false)], false)]

 *)

let table vars expr =
  let rec aux bindings expr = function
    | [] -> begin match evalexpr bindings expr with
            | Some value -> [bindings,value]
            | None -> []
            end
    | var::t -> (aux ((var,false)::bindings) expr t) @ (aux ((var,true)::bindings) expr t)
  in aux [] expr vars ;;
