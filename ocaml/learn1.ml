(* Playing around to learn OCaml... *)

(*
for i = 0 to 500 do
  Graphics.plot i (Random.int i)
done
*)

let rec length list =
  match list with
  | [] -> 0
  | head :: tail -> 1 + (length tail)

let rec min2 list =
  match list with
  | [] -> failwith "Not defined for empty list"
  | [a] -> a
  | a :: rest -> min a (min2 rest)

let rec min1 list =
  match list with
  | [] -> 0
  | [a] -> a
  | a :: b :: tail -> min1 ((min a b) :: tail)



                          


(*
for i = 0 to 400 do
 let a = 0. +. (float i) *. 0.01 in
  clear_graph () ;
  for i = 0 to 40000 do 
    let z = float i *. 0.0005 in
    let x = (500 + (Int.of_float (400. *. (sin z)))) in
    let y = (500 + (Int.of_float (400. *. (cos (z *. a))))) in
    if i = 0 then (moveto x y) ;
    lineto x y
  done ;
  Thread.delay 0.02 ;
done
;;
*)
