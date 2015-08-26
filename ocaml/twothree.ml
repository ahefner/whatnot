(* Crude persistent 2-3 Tree *)

type 'a ttt =
  | Two of 'a ttt * 'a * 'a ttt
  | Three of 'a ttt * 'a * 'a ttt * 'a * 'a ttt
  | Empty ;;

let rec find key = function
  | Empty -> false
  | Two (_,x,_) when x = key -> true
  | Two (l,x,r) -> if (key<x) then find key l else find key r
  | Three (_,x,_,_,_) when x = key -> true
  | Three (_,_,_,y,_) when y = key -> true
  | Three (_,x,m,y,_) when (x < key && key < y) -> find key m
  | Three (l,x,_,y,r) -> if (key < x) then find key l else find key r ;;

(* TODO: Implement insert/delete operations*)

(* Tests  *)

assert (true == find 1 (Two (Empty,1,Empty))) ;;
assert (false == find 7 (Two (Empty,0,Empty))) ;;

let tr1 = (Two
             ((Three (Two (Empty,1,Empty),
                     3,
                     Two (Empty,5,Empty),
                     7,
                     Two (Empty,9,Empty))),
              11,
              (Two (Empty,15,Empty)))) ;;

assert (not (find 0 tr1));;
assert ((find 1 tr1));;
assert (not (find 2 tr1));;
assert ((find 3 tr1));;
assert (not (find 4 tr1));;
assert ((find 5 tr1));;
assert (not (find 6 tr1));;
assert ((find 7 tr1));;
assert (not (find 8 tr1));;
assert ((find 9 tr1));;
assert (not (find 10 tr1));;
assert ((find 11 tr1));;
assert (not (find 12 tr1));;
assert ((find 15 tr1));;
