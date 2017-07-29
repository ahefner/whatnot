(* HUffman Code

We consider a set of symbols with their frequencies. For example, if
the alphabet is "a",..., "f" (represented as the positions 0,...5) and
respective frequencies are 45, 13, 12, 16, 9, 5:

# let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16);
             ("e", 9); ("f", 5) ];;
val fs : (string * int) list =
  [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)]

Our objective is to construct the Huffman code c word for all symbols
s. In our example, the result could be:

 hs = [("a", "0"); ("b", "101"); ("c", "100"); ("d", "111"); ("e", "1101"); ("f", "1100")] (or hs = [ ("a", "1");...]).

The task shall be performed by the function huffman defined as
follows: huffman(fs) returns the Huffman code table for the frequency
table fs.
 *)

type huff = Empty | Leaf of string | Branch of huff * huff ;;

let rec huffcodes prefix = function
  | Empty -> []
  | Leaf x -> [(x,prefix)]
  | Branch (l,r) -> (huffcodes (prefix^"0") l) @ (huffcodes (prefix^"1") r) ;;

let huffman items =
  let sortem = List.sort (fun (w1,_) (w2,_) -> compare w1 w2)
  in let join (w1,x) (w2,y) = (w1+w2, Branch (x,y))
     in let rec aux = function
          | [] -> Empty
          | [(_,tree)] -> tree
          | a::b::t ->
             aux (sortem ((join a b)::t))
        in huffcodes "" (aux (sortem (List.map (fun (item,weight) -> (weight, Leaf item)) items))) ;;
