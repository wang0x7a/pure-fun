(* Exercise 2.5
   Sharing can also be useful within a single object, not just between objects.
   For example, if the two subtrees of a given node are identical, then they 
   can be represented by the same tree.

   (a) Use this idea, write a function complete of type elem * int -> tree
       where complete (x, d) creates a complete binary tree of depth d with 
       x stored in every node. This function should run in O(d) time. (This 
       function can be useful as an auxiliary function for abstractions, such
       as bags.)

   (b) Extend this function to create balanced trees of arbitrary size. These 
       trees will not always be complete binary trees, but should be as 
       balanced as possible: for any given node, the two subtrees should 
       differ in size by at most one. This function should run in O(lg n) time.
       (Hint: use a helper function create2 that, given a size m, creates a
       pair of trees, one of size m and one of size m+1.)
 *)

type 'a bst = Nil | Tree of 'a bst * 'a * 'a bst

let complete (x : 'a) (depth : int) : 'a bst =
  let rec loop (n : int) (subtree : 'a bst) : 'a bst =
    if n = 1 then (Tree (subtree, x, subtree))
    else loop (n - 1) (Tree (subtree, x, subtree))
  in loop depth (Tree (Nil, x, Nil))
;;

let rec balanced (x : 'a) (n : int) : 'a bst =
  if n = 0 then Nil
  else if n = 1 then Tree (Nil, x, Nil)
  else if (n - 1) mod 2 = 0 then
    let subtree = balanced x (n / 2) in
    (* We can only copy nodes or subtrees in a same level or height *)
    Tree (subtree, x, subtree)
  else
    let half = (n - 1) / 2 in
    let left = balanced x half in
    let right = balanced x (half + 1) in
    Tree (left, x, right)
;;
