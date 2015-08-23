(* Logic and Codes

Let us define a small "language" for boolean expressions containing variables:
 *)

type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

(* A logical expression in two variables can then be written in prefix
notation. For example, (a or b) and (a and b) is written:

# And(Or(Var "a", Var "b"), And(Var "a", Var "b"));;
- : bool_expr = And (Or (Var "a", Var "b"), And (Var "a", Var "b"))
   *)

(* Truth tables for logical expressions (2 variables). (medium)

Define a function, table2 which returns the truth table of a given
logical expression in two variables (specified as arguments). The
return value must be a list of triples containing (value_of_a,
balue_of_b, value_of_expr).

# table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")));;
- : (bool * bool * bool) list =
[(true, true, true); (true, false, true); (false, true, false);
 (false, false, false)]
 *)

let rec evalexpr env expr =
  let rec lookup var = function
    | [] -> None
    | (bound,value)::_ when bound=var -> Some value
    | (_,_)::t -> lookup var t
  in match expr with
     | Var s -> lookup s env
     | Not s -> begin match (evalexpr env s) with
                      | Some b -> Some (not b)
                      | None -> None
                end;
     | And (s,t) -> begin match ((evalexpr env s),(evalexpr env t)) with
                          | Some a, Some b -> Some (a && b)
                          | _ -> None
                    end
     | Or (s,t) -> begin match ((evalexpr env s),(evalexpr env t)) with
                         | Some a, Some b -> Some (a || b)
                         | _ -> None
                   end ;;

let table2 a b expr =
  let row (a,b) = match (evalexpr [("a",a);("b",b)] expr) with
    | Some x -> [(a,b,x)]
    | None -> []
  in row(true,true) @ row(true,false) @ row(false,true) @ row(false,false) ;;
