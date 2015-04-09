(* This is intended as helpful resource, no warranties! The official
   assignment definition is the one by the prof! *)
exception NotImplemented

(* Question 1 *)

datatype exp = 
	 Nat of int 
       | Bool of bool 
       | Plus of exp * exp
       | Minus of exp * exp
       | Mult of exp * exp 
       | If of exp * exp * exp 
       | And of exp * exp 
       | Not of exp 
       | Eq of exp * exp 
       | Var of string 
       | Let of exp * (string * exp) 
       | Fun of string * string * exp 
       | Apply of exp * exp


fun free_list (e : exp) : string list =
    raise NotImplemented

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
		 Plus(Nat(1),Apply(Var("g"),(Minus(Var("n"),Nat(1)))))))
                 (* no free variables *)

(* Question 2 *)

fun Catalan (n : int) : int = 
    raise NotImplemented

(* The results for several n's are in the assignment, the fourth
   should be 5 for example *)

datatype realSeq = Cons of real * (unit -> realSeq)

fun take n s = case (n , s) of
  (0, Cons (x, f)) => []
| (n, Cons (x, f)) => x :: (take (n-1) (f ()))

fun helperSeq (n : int) : realSeq = 
    raise NotImplemented

(* val catalanSeq : realSeq = raise NotImplemented *)

(* take 5 catalanSeq should return [1, 1,  2, 5, 15] *)

(* Question 3 *)

datatype 'a instr = Put of 'a | Get | Restore

fun makeCell (x : 'a) : ('a instr -> 'a) = 
    raise NotImplemented


(* Example interaction (fixes a typo in the assignment):

- val cell0 = makeCell 0;
val cell0 = fn : int instr -> int
- cell0 (Put(5));
val it = 5 : int
- cell0 (Put(8));
val it = 8 : int
- cell0 (Get);
val it = 8 : int  (* <--- This was missing *)
- cell0 (Restore);
val it = 8 : int
- cell0 (Restore);
val it = 5 : int
- cell0 (Restore);
val it = 0 : int
- cell0 (Restore);
uncaught exception Error

*)

