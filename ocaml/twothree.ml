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
     in findin n ;;

(* Recursive three element bubble sort? :) *)
let rec sortedtriple x y z =
  if x > y then sortedtriple y x z
  else if y > z then sortedtriple x z y
  else (x,y,z) ;;

let trio x y z =
  let (a,b,c) = sortedtriple x y z
  in Two (Leaf a, b, Leaf c) ;;

(* Insertion into 2-3 tree *)

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

(* Removal from 2-3 Tree *)

(* OMFG, no wonder nobody implements 2-3 trees directly and every
reference glosses over how the deletion works. *)

(*
type 'a remop =
  | Child of 'a ttnode          (* No rotation required *)
  | Hole                        (* Empty leaf *)
  | Orphans of 'a * 'a (* Orphaned leaves of removed Two-node *)
;;

let rec removing key = function
  | Leaf x when x=key -> Hole
  | TwoLeaf (x,k) when k=key -> Child (Leaf x)
  | TwoLeaf (k,x) when k=key -> Child (Leaf x)
  | Leaf x -> Child (Leaf x)
  | TwoLeaf xy -> Child (TwoLeaf xy)
  | Two (l,x,r) when x=key ->
     begin
       match (l,r) with
       | (Leaf l, Leaf r) -> Orphans (l,r)
       | (TwoLeaf (a,b), Leaf c) | (Leaf a, TwoLeaf (a,b)) -> Child (trio a b c)
       | (TwoLeaf (a,b), TwoLeaf (c,d)) -> Child (Two (Leaf (min a b), max a b, TwoLeaf (c,d))) (* arbitrary choice? *)
       | (l,r) ->
          let 
     end
       
  | Two (Leaf l, x, Leaf r) when x=key -> Orphans (l,r)
  | Two (TwoLeaf (a,b), x, c) when x=key -> Child (Two ((Leaf (min a b)), (max a b), c))
  | Two (a, x, TwoLeaf(b,c)) when x=key -> Child (Two (a, (min b c), Leaf (max b c)))
;;


  | Two (l,x,r) when x=k -> Orphans (l,r)
  | 

let remove key = function
  | Empty -> Empty
  | Root root = match removing key root with
                | Hole -> Empty

 *)

(* TODO: Implement delete operation*)

(* TODO: Write tests *)




