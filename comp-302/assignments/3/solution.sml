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

(* Problem 2 *)

fun pow n 0 = 1
  | pow n k = n * pow n (k - 1)

fun pow_tl n 0 acc = acc
  | pow_tl n k acc = pow_tl n (k - 1) (n * acc)

(*
* We observe that for 0 <= i <= k, the accumulator of (pow_tl n i acc) equals
* pow n (k - i), and so we find the following invariant, which we will prove.

pow_tl n i acc = acc * pow n i

* Base case, i = 0:

LHS = pow_tl n 0 acc = acc (by base case definition of pow_tl)

RHS = acc * pow n 0 = acc * 1 (by base case definition of pow)
    = acc (since 1 is the identity of multiplication)

We observe that LHS = RHS, and so the base case is verified.

* Inductive case, i => i + 1:
* Assume the following:

pow_tl n i acc = acc * pow n i

* then,

pow_tl n (i + 1) acc
  = pow_tl n i (n * acc) (by inductive definition of pow_tl)
  = (n * acc) * pow n i (by inductive hypothesis)
  = acc * (n * pow n i) (by associativity and commutativity of multiplication)
  = acc * pow n (i + 1) (by inductive definition of pow)
  q.e.d.

* With this formula, we will examine some key applications of pow_tl and pow to
* establish that they produce the same results.
* If we call pow_tl with acc=1, then we get the result we want:

pow_tl n k 1 = 1 * pow n k = pow n k (since 1 is the identity of multiplication)

* This equality is exactly the desired one, namely that the functions are
* equivalent when they are called with the same arguments, provided that the
* initial value for the accumulator is 1.
*)

(* Problem 3 *)

type 'a church = ('a -> 'a) * 'a -> 'a


(* Problem 3.1 *)

(* The base case is that the function is applied zero times, so we yield the
* base value. *)
fun create 0 = (fn (_, x) => x)
  | create n =
  let
    (* In the inductive case, we create the church numeral for one less... *)
    val c = create (n - 1)
  in
    (* and essentially reimplement the successor function defined below. *)
    fn (f, x) => f (c (f, x))
  end

(* Problem 3.2 *)

(* Use a counting function to extract the number of function calls embedded in
* the church numeral. *)
fun churchToInt c = c (fn x => x + 1, 0)

(* Problem 3.3 *)

(* The church numeral given to us performs f(f(...f(x)...)) for n applications
* of f, where we supply it with a given (f, x) tuple. So to find its successor,
* we have to construct a lambda taking an (f, x) tuple, call the church numeral
* on it, and then call f one more time on the result, to get the n+1-th
* application. *)
fun succ c = fn (f, x) => f (c (f, x))
