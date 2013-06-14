(* Exercise 2.2
 * In the worst case, member performs approximately 2d comparisons, where d is
 * the depth of the tree. Rewrite member to take no more than d + 1 comparisons
 * by:
 * 1) keeping track of a candidate element that might be equal to the query 
 *    element, and
 * 2) chekcing our equality only when you hit the bottom of the tree.
 *)

type 'a bst = Nil | Tree of 'a bst * 'a * 'a bst

let rec insert (x : 'a) (tree : 'a bst) : 'a bst =
  match tree with
  | Nil -> Tree (Nil, x, Nil)
  | Tree (r, y, l) ->
    if x > y then Tree (r, y, insert x l)
    else if x < y then Tree (insert x r, y, l)
    else Tree (r, y, l)

let member (x : 'a) (tree : 'a bst) : bool =
  let rec loop (tmp : 'a) (subtree : 'a bst) : bool =
    match subtree with
    | Nil -> x = tmp  
    (*| Tree (Nil, y, Nil) -> x = tmp*)
    | Tree (r, y, l) ->
      if x > y then loop tmp l
      else loop y r
  in loop 0 tree
;;
