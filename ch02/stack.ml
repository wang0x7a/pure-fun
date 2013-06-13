module type STACK =
  sig
    type 'a stack

    exception EmptyStack

    val empty   : 'a stack
    val isEmpty : 'a stack -> bool

    val push    : 'a -> 'a stack -> 'a stack
    val pop     : 'a stack -> 'a stack
    val top     : 'a stack -> 'a

    val map     : ('a -> 'b) -> 'a stack -> 'b stack
  end

module ListStack : STACK =
  struct
    type 'a stack = 'a list
    exception EmptyStack

    let empty : 'a stack = []
    let isEmpty (s : 'a stack) : bool = s = []

    let push (x : 'a) (s : 'a stack) : 'a stack = x :: s

    let pop (s : 'a stack) : 'a stack =
      match s with
        [] -> raise EmptyStack
      | x :: xs -> xs

    let top (s : 'a stack) : 'a =
      match s with
        [] -> raise EmptyStack
      | x :: xs -> x

    let map (f : 'a -> 'b) (s : 'a stack) : 'b stack = List.map f s

  end

module CustomStack : STACK =
  struct
    type 'a stack = Nil | Cons of 'a * 'a stack
    exception EmptyStack

    let empty : 'a stack = Nil
    let isEmpty (s : 'a stack) : bool = s = Nil

    let push (x : 'a) (s : 'a stack) : 'a stack = Cons (x, s)
    let pop (s : 'a stack) : 'a stack = 
      match s with
      | Nil -> raise EmptyStack
      | Cons (_, xs) -> xs

    let top (s : 'a stack) : 'a =
      match s with
      | Nil -> raise EmptyStack
      | Cons (x, _) -> x
    
(*
    type 'b stack = Nilb | Consb of 'b * 'b list

    let map (f : 'a -> 'b) (s : 'a stack) : 'b stack =
      let rec aux (acc : 'b list) = function
      | Nilb -> acc
      | Cons (x, xs) -> aux (Consb (f x, acc)) xs
      in aux Nilb s
*)      
  end
