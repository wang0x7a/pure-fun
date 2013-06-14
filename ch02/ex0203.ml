(* Exercise 2.3
 * Inserting an existing element into a binary search tree copies the entire
 * search path even though the copied nodes are indistinguishable from the 
 * originals. Rewrite insert using exceptions to avoid this copying. Establish
 * only one handler per insertion rather than one handler per iteration.
 *)

type 'a bst = Nil | Tree of 'a bst * 'a * 'a bst

exception ExistingElement

let rec insert (x : 'a) (tree : 'a bst) : 'a bst =
  match tree with
  | Nil -> Tree (Nil, x, Nil)
  | Tree (l, y, r) ->
    (* One of the keys to understanding functional data structures is
     * the copying-sharing process. For example,
     * A copy is made by constructing a new Tree (Tree (l, y, insert x r)),
     * and l and y, in this example, are shared by the copy.
     *)
    if x > y then Tree (l, y, insert x r)
    else if x < y then Tree (insert x l, y, r)
    else raise ExistingElement
;;
