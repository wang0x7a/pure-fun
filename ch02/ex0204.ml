(* Exercise 2.4
 * Reimplement a version of insert that performs no unnecessary copying and 
 * uses no more than d + 1 comparisons.
 *)

type int bst = Nil | Tree of int bst * int * int bst

exception ExistingElement

let insert (x : int) (tree : 'a bst) : int bst =
  let rec loop (candidate : int) (subtree : int bst) : int bst =
    match subtree with
    | Nil ->
      if candidate = x then raise ExistingElement
      else Tree (Nil, x, Nil)
    | Tree (left, y, right) ->
      if x <= y then Tree (loop y left, y, right)
      else Tree (left, y, loop candidate right)
  in loop 0 tree
;;
