(* Exercise 2.1
   Write a function suffixes of type 'a list -> 'a list list that takes a
   list xs and returns a list of all the suffixes of xs in decreasing order
   of length. For example,

   suffixes[1;2;3;4] = [[1;2;3;4]; [2;3;4]; [3,4]; [4]; []]

   Requirements: O(n) time and O(n) space
 *)

let rec suffixes (l : 'a list) : 'a list list =
  match l with
  | [] -> [[]]
  | _ :: t -> [l] @ (suffixes t)
;;

(* Analysis:
   Time complexity: O(n)
   Space complexity: O(n) (the resursion will consume a n-depth stack)
 *)

(* TCO:
   Use tail recursion to reduce the depth of the stack, and hence, 
   the space complexity becomes O(1).
 *)
let suffixes (l : 'a list) : 'a list list =
  let rec aux (acc : 'a list list) = function
  | [] -> acc @ [[]]
  | (_ :: t) as lst -> aux (acc @ [lst]) t
  in aux [] l
;;
