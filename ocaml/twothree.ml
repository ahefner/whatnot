(* Crude persistent 2-3 Tree *)

(* Interior node or leaf of non-empty 2-3 tree *)
type 'a ttnode =
  | Two of 'a ttnode * 'a * 'a ttnode
  | Three of 'a ttnode * 'a * 'a ttnode * 'a * 'a ttnode
  | Leaf of 'a
  | TwoLeaf of 'a * 'a  (* Not ordered *)
;;

(* 2-3 Tree *)
type 'a ttt = Empty | Root of 'a ttnode ;;

let rec find key = function
  | Empty -> false
  | Root n ->
     let rec findin = function
       | Two (_,x,_) when x = key -> true
       | Two (l,x,r) -> if (key < x) then findin l else findin r
       | Three (_,x,_,_,_) when x = key -> true
       | Three (_,_,_,y,_) when y = key -> true
       | Three (_,x,m,y,_) when (x < key && key < y) -> findin m
       | Three (l,x,_,y,r) -> if (key < x) then findin l else findin r
       | Leaf (x) -> key = x
       | TwoLeaf (x,y) -> (key = x || key = y)
     in findin n
;;

(* Recursive three element bubble sort? :) *)
let rec sortedtriple x y z =
  if x > y then sortedtriple y x z
  else if y > z then sortedtriple x z y
  else (x,y,z) ;;

type 'a insertop = ReplaceWith of 'a ttnode | MergeUp of 'a ttnode * 'a * 'a ttnode ;;

let joinleft = function
  | (ReplaceWith l, x, r) -> ReplaceWith (Two (l,x,r))
  | (MergeUp (l,x,m), y, r) -> ReplaceWith (Three (l,x,m,y,r)) ;;

let joinright = function
  | (l, x, ReplaceWith r) -> ReplaceWith (Two (l,x,r))
  | (l, x, MergeUp (m,y,r)) -> ReplaceWith (Three (l,x,m,y,r)) ;;

let join2left = function
  | (ReplaceWith l, x, m, y, r) -> ReplaceWith (Three (l,x,m,y,r))
  | (MergeUp (l,x1,m1), x2, m2, y, r) -> MergeUp (Two (l,x1,m1), x2, Two (m2,y,r)) ;;

let joinmiddle = function
  | (l, x, ReplaceWith m, y, r) -> ReplaceWith (Three (l, x, m, y, r))
  | (l, x1, MergeUp (m1,y,m2), z, r) -> MergeUp (Two (l,x1,m1), y, Two(m2,z,r)) ;;

let join2right = function
  | (l, x, m, y, ReplaceWith r) -> ReplaceWith (Three (l, x, m, y, r))
  | (l, x, m1, y, MergeUp (m2,z,r)) -> MergeUp (Two (l,x,m1), y, Two (m2,z,r)) ;;

let rec inserting key = function
  | Leaf x as itself when x=key -> ReplaceWith itself
  | TwoLeaf (x,y) as itself when x=key || y=key -> ReplaceWith itself
  | Two (_,x,_) as itself when (key = x) -> ReplaceWith itself
  | Three (_,x,_,y,_) as itself when x=key || y=key -> ReplaceWith itself
  | Leaf x -> ReplaceWith (TwoLeaf (x,key))
  | TwoLeaf (x,y) ->
     let (x,y,z) = sortedtriple x y key
     in MergeUp (Leaf x, y, Leaf z)
  | Two (l,x,r) ->
     if key < x then joinleft ((inserting key l), x, r)
     else joinright (l, x, (inserting key r))
  | Three (l,x,m,y,r) ->
     if (key < x) then join2left ((inserting key l),x,m,y,r)
     else if (key < y) then joinmiddle (l,x,(inserting key m),y,r)
     else join2right (l,x,m,y,(inserting key r)) ;;

let insert key = function
  | Empty -> Root (Leaf key)
  | Root root -> match inserting key root with
                 | ReplaceWith newroot -> Root newroot
                 | MergeUp (l,x,r) -> Root (Two (l,x,r)) ;;

(* TODO: Implement delete operation*)

(* TODO: Write tests *)




