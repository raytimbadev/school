(* Problem 1 *)

datatype 'a tree = Empty | Node of (int * 'a) * 'a tree * 'a tree

exception Found of (int * string) list

fun insert (k, v) Empty = Node ((k, v), Empty, Empty)
  | insert (k, v) (Node ((k', v'), left, right)) =
  if k < k'
  then Node ((k', v'), insert (k, v) left, right)
  else Node ((k', v'), left, insert (k, v) right)

val testTree = (insert (3, "hi") o insert (1, "hi") o insert (2, "lol") o
  insert (5, "hi") o insert (4, "lol") o insert (6, "yo")) Empty


(* Problem 1.1 *)

fun collect p t =
let
  fun go p Empty = []
    | go p (Node ((k, s), left, right)) =
    let
      (* traverse the left side and capture any found nodes *)
      val foundLeft = go p left handle Found vs => vs
      (* traverse the right side and capture any found nodes *)
      val foundRight = go p right handle Found vs => vs
    in
      if p k
      then raise Found (foundLeft @ (k, s) :: foundRight)
      else raise Found (foundLeft @ foundRight)
    end
in
  go p t handle Found vs => vs
end

val testCollect = collect (fn x => x < 4) testTree


(* Problem 1.2 *)

fun gather p t k =
  case t
    of Empty => k []
     | Node ((k', v), left, right) =>
         if p k'
         then gather p left (fn _ => (k', v) :: gather p right k)
         else gather p left (fn _ => gather p right k)

val testGather = gather (fn x => x < 4) testTree (fn x => x)
