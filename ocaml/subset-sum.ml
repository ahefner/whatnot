
(* Subset sum (dynamic programming) *)

let sss = function
  | [] -> false
  | x0 :: t as list ->
     let a = List.fold_left (fun y x -> if x < 0 then x+y else y) 0 list in
     let b = List.fold_left (fun y x -> if x > 0 then x+y else y) 0 list in
     let arr = Array.make_matrix (List.length list) (1+b-a) false in
     let set i s x = arr.(i).(s-a) <- x in
     let get i s = if s>=a && s<=b then arr.(i).(s-a) else false in
     for s = a to b do set 0 s (x0==s) done ;
     let rec aux i = function
       | [] -> ()
       | xi :: xs ->
          begin
            for s = a to b do
              set i s ((get (i-1) s) || (xi == s) || get (i-1) (s-xi))
            done ;
            aux (i+1) xs
          end
     in
     aux 1 t ;
     get ((List.length list)-1) 0
;;











