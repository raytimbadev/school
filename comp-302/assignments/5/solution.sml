(* Problem 1 *)

datatype exp = Apply of exp * exp
             | If of exp * exp * exp
             | Let of exp * (string * exp) (* introduces one binding *)
             | Fun of string * string * exp (* introduces two bindings *)
             | Plus of exp * exp
             | Mult of exp * exp
             | And of exp * exp
             | Eq of exp * exp
             | Not of exp
             | Var of string
             | Nat of int
             | Bool of bool

(* a type to represent bound variables *)
type Binding = string

(* a type to act as a stack of bindings *)
type Environment = Binding list

(* We can use a simple stack to represent the evaluation environment because in
* our toy language, each constructor of exp can only introduce a fixed number of
* bindings at a time. If it were possible for constructors to introduce a
* variadic number of bindings, then we would introduce a third type envstack =
* env list.
*)

(* The insertion sort code is from Wikipedia. *)
fun ins' << (num, nums) = 
let
  fun i (n, []) = [n]
    | i (n, ns as h::t) = if <<(n,h) then n::ns else h::i(n,t)
  in
    i (num, nums)
  end

fun insertionSort' << = List.foldr (ins' <<) []

(* A straightforward function to eliminate duplicates from a sorted list. *)
fun uniq [] = []
  | uniq [x] = [x]
  | uniq (x::y::xs) = if x = y then uniq (x::xs) else x :: uniq (y::xs)

(* There's a wonderful function in the SML/NJ libraries called uniqueSort that
sorts a list and gets rid of duplicates. Turns out I can't figure out how to do
imports in this language, so I've reimplemented it (inefficiently) with a
combination of the above insertion sort and the uniq function. *)
fun uniqueSort cmp = uniq o insertionSort' cmp

(* A helper function to check if an item is in a list. *)
fun elem (x : ''a) (ys : ''a list) : bool =
  case ys
    of [] => false
     | (y::ys) => if x = y then true else elem x ys

fun free_list (e : exp) : string list =
let
  fun go env vars (Bool _) = vars
    | go env vars (Nat _) = vars
    | go env vars (Var s) =
    if elem s env
    then vars (* s is bound, so we return the list of freevars unchanged *)
    else s :: vars (* s is free, so we prepend it to the list of freevars *)
    | go env vars (Not e) = go env vars e (* no bindings are introduced *)
    | go env vars (Eq (e1, e2)) =
    let
      val vars' = go env vars e1
    in
      go env vars' e2
    end
    | go env vars (And (e1, e2)) =
    let
      val vars' = go env vars e1
    in
      go env vars' e2
    end
    | go env vars (Mult (e1, e2)) =
    let
      val vars' = go env vars e1
    in
      go env vars' e2
    end
    | go env vars (Plus (e1, e2)) =
    let
      val vars' = go env vars e1
    in
      go env vars' e2
    end
    | go env vars (Fun (name, v, e)) = go (v::name::env) vars e
    | go env vars (Let (e1, (v, e2))) =
    let
      val vars' = go env vars e1
    in
      go (v::env) vars' e2
    end
    | go env vars (If (e1, e2, e3)) =
    let
      val vars' = go env vars e1
      val vars'' = go env vars' e2
    in
      go env vars'' e2
    end
    | go env vars (Apply (e1, e2)) =
    let
      val vars' = go env vars e1
    in
      go env vars' e2
    end
in
  uniqueSort (op <) (go [] [] e)
end

val ex1 = Let(Nat(5),("a",(Plus(Var("a"),Nat(2))))) (* No free variables *)
val ex2 = Plus(Var("a"),Nat(2))			    (* a is free here *)
val ex3 = Let(Var("y"),("x",Plus(Var("x"),Var("z")))) (* y and z are free here *)
val ex4 = Let(Nat(3),("z",(Let(Nat(2),("y",ex3)))))   (* no free variables *)
val ex5 = Fun("fac","n",
	      If(Eq(Var("n"),Nat(0)),Nat(1),Mult(Var("n"),(Apply(Var("fac"),
		 (Mult(Var("n"),Nat(1)))))))) (* no free variables *)
val ex6 = Fun("f","n",Plus(Nat(3),Var("n"))) (* no free variables *)
val ex7 = Fun("g","n",
	      If(Eq(Var("n"),Nat(0)),Nat(0),
		 Plus(Nat(1),Apply(Var("g"),(Plus(Var("n"),Nat(~1)))))))
                 (* no free variables *)

val test1 = null (free_list ex1)
val test2 = free_list ex2 = ["a"]
val test3 = free_list ex3 = ["y", "z"]
val test4 = null (free_list ex4)
val test5 = null (free_list ex5)
val test6 = null (free_list ex6)
val test7 = null (free_list ex7)

(* Problem 2 *)

(* Basically equivalent to the Haskell notation [1..(n-1)] *)
fun range n =
let
  fun go i = if i < n then i :: go (i + 1) else []
in
  go 0
end

(* n is the index of cn in the list of Catalan numbers *)
fun nextCatalan (n, cn) = (2*(2*n + 1)*cn) div (n+2)
(* The product with `cn` needs to occur in the numerator, before division,
* otherwise the numerator won't be divisible by the denominator, and truncation
* will skew the results.
*)

(* Part 1 *)

(* folding over the function that takes us to the next catalan number produces
* the desired result. *)
fun catalan n = foldl nextCatalan 1 (range n)

(* Part 2 *)

datatype realSeq = Cons of real * (unit -> realSeq)

(* The following solution is in my opinion a lot more straightforward than one
* involving a helper function to create the stream of coefficients.
* I'll give a solution involving the helper function a bit later. 
*)

(* The nextCatalan function, defined on reals. *)
fun nextCatalanR n cn = (2.0*(2.0*n + 1.0)*cn) / (n + 2.0)

val catalans =
let
  fun go n cn =
  let
    val cn' = nextCatalanR n cn
  in
    Cons (cn', fn () => go (n + 1.0) cn')
  end
in
  Cons (1.0, fn () => go 0.0 1.0)
end

(* A solution using the required helper function. *)

fun helperSeq n = 
  Cons (
    2.0*(2.0 * Real.fromInt n + 1.0) / (Real.fromInt n + 2.0),
    fn () => helperSeq (n + 1)
  )

val catalansH = 
let
  fun go cn (Cons (k, f)) = 
  let
    val cn' = k * cn
  in
    Cons (cn', fn () => go cn' (f ()))
  end
in
  Cons(1.0, fn () => go 1.0 (helperSeq 0))
end

(* A convenience function for converting lazy steams into lists. *)
fun seqTake 0 _ = []
  | seqTake n (Cons (cn, f)) = cn :: seqTake (n-1) (f ())

(* Given a lazy stream of Catalan numbers, check that the first 10 values
* correspond to the ones generated by the recurrence-based method.
*)
fun testCatalansStream s = 
let
  val s1 = map round (seqTake 10 s) 
  val s2 = map catalan (range 10)
in
  s1 = s2
end

val testMyCatalanStream = testCatalansStream catalans
val testHelperCatalanStream = testCatalansStream catalansH

(* Problem 3 *)

datatype 'a instr = Put of 'a | Get | Restore

exception NoData

fun makeCell initialValue =
let
  val data = ref [initialValue]
  fun f (Put n) = 
  let
    val l = !data
  in
    data := n::l;
    n
  end
    | f Get =
  let
    val l = !data
  in
    if null l
    then (print "Nothing to get" ; raise NoData)
    else hd l
  end
    | f Restore =
  let
    val l = !data
  in
    if null l
    then (print "Nothing to restore" ; raise NoData)
    else let val (x::xs) = l in (data := xs ; x) end
  end
in
  f 
end
