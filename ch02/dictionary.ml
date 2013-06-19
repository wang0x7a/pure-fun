(* Exercise 2.6
   Adapt the UnbalancedSet functor to support finite maps (dictionaries) 
   rather than sets.
 *)

(**
 Implement a complete functor, including the type of accepted module, and
 the encapsulated returned type with the syntax sugar of "... with type ...".
 *)

module type ORDERED = sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end

module type FINITE_MAP = sig
  type key
  type 'a dict

  exception NotFound

  val make : unit -> 'a dict
  val insert : 'a dict -> key -> 'a -> 'a dict
  val lookup : 'a dict -> key -> 'a
end

module type DICTIONARY =
  functor (Element : ORDERED) -> FINITE_MAP with type key = Element.t

(* Various implmentations of DICTIONARY referring to CS3110 at Cornell. *)

(**
 * 1. Association lists
 *   [(k1, v1); (k2, v2); ...; (kn, vn)]
 *)
module AssocListFn : DICTIONARY =
  functor (Element : ORDERED) ->
    struct
      type key = Element.t
      type 'a dict = (key * 'a) list

      exception NotFound

      let make () : 'a dict = []

      let insert (d : 'a dict) (k : key) (v : 'a) : 'a dict = (k, v) :: d

      let rec lookup (d : 'a dict) (k : key) : 'a =
        match d with
        | [] -> raise NotFound
        | (k', v') :: t ->
          if k = k' then v'
          else lookup t k 
    end

(**
 * 2. Sorted Association Lists (in increasisng order) 
 *)
module SortedAssocListFn : DICTIONARY =
  functor (Element : ORDERED) ->
    struct
      type key = Element.t
      type 'a dict = (key * 'a) list

      exception NotFound
      exception ExistingElement

      let make () : 'a dict = []

      (* Sorting elements in increasing order *)
      let rec insert (d : 'a dict) (k : key) (v : 'a) : 'a dict =
        match d with
        | [] -> (k, v) :: []
        | (k', v') :: t ->
          (* Avoid copying identical nodes *)
          if Element.eq k k' then raise ExistingElement
          else if Element.lt k k' then (k, v) :: d
          else (k', v') :: (insert t k v)
          
      let rec lookup (d : 'a dict) (k : key) : 'a =
        match d with
        | [] -> raise NotFound
        | (k', v') :: t ->
          if Element.eq k k' then v'
          else if Element.lt k k' then raise NotFound
          else lookup t k
    end

(**
 * 3. Associated Tree
 *    Use a binary search tree to store the data, with O(lg n) time in
 *    both insert and lookup functions.
 *)
module BinarySearchTreeFn : DICTIONARY =
  functor (Element : ORDERED) ->
    struct
      type key = Element.t
      type 'a dict = Empty | Node of key * 'a * 'a dict * 'a dict

      exception NotFound
      exception ExistingElement

      let make () : 'a dict = Empty

      let rec insert (d : 'a dict) (k : key) (v : 'a) : 'a dict =
        match d with
        | Empty -> Node (k, v, Empty, Empty)
        | Node (k', v', left, right ) ->
          if Element.eq k k' then raise ExistingElement
          else if Element.lt k k' then Node (k', v', insert left k v, right)
          else Node (k', v', left, insert right k v)

      let rec lookup (d : 'a dict) (k : key) : 'a =
        match d with
        | Empty -> raise NotFound
        | Node (k', v', left, right) ->
          if Element.eq k k' then v'
          else if Element.lt k k' then lookup left k
          else lookup right k
    end

(**
 * 4. Functional Dictorinary
 *    Use higher order functions to record data
 *)
module FunctDictFn : DICTIONARY =
  functor (Element : ORDERED) ->
    struct
      type key = Element.t
      (* 'a dict here is no longer data (in imperative sense),
       * instead, it is a function to map a given key to its value.
       * Hence, the order of this function grows along with the number 
       * of elements.
       *)
      type 'a dict = key -> 'a

      exception NotFound

      let make () = fun _ -> raise NotFound

      let lookup (d : 'a dict) (k : key) : 'a = d k

      let insert (d : 'a dict) (k : key) (v : 'a) : 'a dict =
        fun k' -> if k = k' then v
                  (* It could be easier to understand the higher-order
                   * mechanism of insert with the knowledge of the 
                   * evaluation model of substitution. In fact, insert
                   * rewrites the origina d with d', i.e.,
                   * d' = fun k' -> if k = k' then v else d k'
                   * ^                                    ^
                   * new dict with higher order        original dict
                   *)
                  else d k'
    end
