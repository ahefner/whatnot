let rev = fun list ->
  let rec revappend rest = function
    | [] -> rest
    | a :: t -> revappend (a :: rest) t
  in revappend [] list ;;




