module type ORDERED = sig
  type t

  val eq  : t -> t -> bool
  val lt  : t -> t -> bool
  val leq : t -> t -> bool
end


module type SET = sig
  type elem
  type set

  val empty   : set
  val insert  : elem -> set -> set
  val member : elem -> set -> bool
end

module type UNBALANCED_SET =
  functor (Element : ORDERED) -> SET with type elem = Element.t

module BinarySearchTreeFn : UNBALANCED_SET =
  functor (Element : ORDERED) ->
    struct
      type elem = Element.t
      type tree = Nil | Tree of tree * elem * tree
      type set = tree

      let empty = Nil

      let rec insert (x : elem) (t : set) : set =
        match t with
        | Nil -> Tree (Nil, x, Nil)
        | Tree (r, y, l) ->
          if Element.lt x y then Tree (insert x r, y, l)
          else if Element.lt y x then Tree (r, y, insert x l)
          else t

      let rec member (x : elem) (t : set) : bool =
        match t with
        | Nil -> false
        | Tree (r, y, l) ->
          if Element.lt x y then member x r
          else if Element.lt y x then member x l
          else true 
    end
;;


(* Test case *)
(* Here, I do not specify the signiture of IntElement,
   in order to expose the int type of t to IntTree.
 *)
module IntElement = struct
  type t = int

  let eq i1 i2 = i1 = i2
  let lt i1 i2 = i1 < i2
  let leq i1 i2 = i1 <= i2
end
;;

module IntTree = BinarySearchTreeFn(IntElement);;

let x = IntTree.empty;;
let x1 = IntTree.insert 1 x;;
let x2 = IntTree.insert 2 x1;;
IntTree.member 2 x2;;
IntTree.member 2 x1;;
