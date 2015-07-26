let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t;;

let rec length_tailrec = fun l ->
  let rec inner count = function
    | [] -> count
    | _ :: t -> inner (1+count) t ;
  in
  inner 0 l ;;

