(* Assignment #2
 * By: Jacob Errington (260636023)
 *)

(* Problem 1 *)

(* The idea is that on each element, we scan the whole tail of the list,
* rebuilding it as we go along, skipping over elements equal to that first one
* in the head.
*)
fun remDuplicates [] = []
  | remDuplicates (x::xs) =
  let
    fun go [] _ = []
      | go (x::xs) y = if x = y then go xs y else x :: go xs y
  in
    x :: go (remDuplicates xs) x
  end

(* Problem 2 *)

exception Diverge

fun newton f x0 e =
let
  (* If the user provides an epsilon value smaller than one, then cubing it
  * gives a good value to use for our derivate approximation. Otherwise, we take
  * 10^-6.
  *)
  val h = if e < 1.0 then e * e * e else 0.000001
  fun derivative f x = (f (x + h) - f (x - h)) / (2.0 * h)

  (* We introduce a helper function to count the iterations, and raise the
  * exception if the counter reaches zero. *)
  fun newtonHelper _ _ _ 0 = raise Diverge
    | newtonHelper f xi e iter =
  let
    val xNext = xi - (f xi / derivative f xi)
  in
    if abs (f xi) < e
    then xi
    else newtonHelper f xNext e (iter - 1)
  end
in
  newtonHelper f x0 e 1000 (* start with 1000 iterations. *)
end

(* Problem 3 *)

(* We use the functions defined in class. *)
fun sum f (a : real, b : real) inc =
if (a > b)
  then 0.0
  else (f a) + (sum f (inc(a), b) inc)

fun integral f (a, b) dx =
dx * sum f (a + dx/2.0, b) (fn x => (x+dx))

val t = integral (fn x => x*x) (0.0, 1.0) 0.01

(* And we write the solution. *)
fun approxH f g dy x =
let
  fun j y = (f y)*(g (x + y))
in
  integral j (0.0, x) dy
end

(* The above is more readable than a one-line solution since it's spread out.
 * If you really want a one liner, here:

  fun approxH f g dy x = let fun j y = (f y)*(g (x + y)) in integral j (0, x) dy end

All I did was squish it up onto one line.

Anyway, it might not look like it "returns a function", but as we learned in
class, all functions of arity n in SML are truly functions of arity 1 that
return functions of arity n-1. So if we call approxH with just 3 arguments,
then it evaluates to a function of one variable, namely x.

Here's some proof:
*)

val proof =
let
  fun f x = x * x (* declare some dummy values for argument's sake *)
  fun g x = (~2.0) * x * x - 5.0
  val dy = 0.0001
in
  approxH f g dy
end

(* The inferred type of proof is real -> real, as required. *)

(* Problem 4 *)

datatype Mathexp = Num of int
                 | Var of string
                 | Neg of Mathexp
                 | Add of Mathexp * Mathexp
                 | Mul of Mathexp * Mathexp

(* Problem 4.1 *)

fun diff (Num _) _ = Num 0 (* Derivative of a constant is zero. *)
  (* For variables, subtract degree by one, and multiply by the old degree.
  * Since we're only using degree-one variables, if the variable matches
  * the one we're differentiating by, then we change it to the constant 1,
  * else it's a constant, and goes to zero.
  *)
  | diff (Var x) v = if x = v then Num 1 else Num 0
  (* Just differentiate the inner expression, and negate that derivative. *)
  | diff (Neg e) v = Neg (diff e v)
  (* Derivative of a sum is the sum of the derivatives. *)
  | diff (Add (e1, e2)) v = Add (diff e1 v, diff e2 v)
  (* Use the product rule. *)
  | diff (Mul (e1, e2)) v = Add (Mul (diff e1 v, e2), Mul (e1, diff e2 v))

(* Simplifier for expressions. It evaluates operations wherever possible.
* Sample simplifications:
* > e + 0 -> e
* > e * 0 -> 0
* > e * 1 -> e
* > n + m -> (n+m)
* > n * m -> (n*m)
* > e * (-1) -> -e
* where
*   e refers to any expression
*   n, m refer to numbers
* Simplification is deep, by first simplifying the operands, and sometimes by
* simplifying a second time upon reconstructing the expression.
*)
fun simplify (Add (e1, e2)) =
  let (* simplify the operands *)
    val s1 = simplify e1
    val s2 = simplify e2
  in
    case s1
      of Num 0 => s2 (* 0 + e -> e *)
       | Num n => (case s2
                     of Num m => Num (n + m) (* n + m -> (n+m) *)
                      | _ => Add (s1, s2) (* cannot be simplified further *)
                  )
       | _ => (case s2
                 of Num 0 => s1 (* e + 0 -> e *)
                  | _ => Add (s1, s2) (* cannot be simplified further *)
              )
  end
  | simplify (Mul (e1, e2)) =
  let (* simplify the operands *)
    val s1 = simplify e1
    val s2 = simplify e2
  in
    case s1
      (* -1 * e -> -e, however resimplification of (-e) is necessary in case e
      * is numeric, since we simplify (Neg (Num m)) to (Num ~m). *)
      of Num (~1) => simplify (Neg s2)
       | Num 0 => Num 0 (* 0 * e -> 0 *)
       | Num 1 => s2 (* 1 * e -> e *)
       | Num n => (case s2
                     of Num m => Num (n * m) (* n * m -> (n*m) *)
                      | _ => Mul (s1, s2) (* cannot be simplified further *)
                  )
       | _ => (case s2
                 of Num (~1) => simplify (Neg s1)
                  | Num 0 => Num 0
                  | Num 1 => s1
                  | _ => Mul (s1, s2)
              )
  end
  | simplify (Neg (Num n)) = Num (~n)
  | simplify e = e (* the remaining cases cannot be simplified (Num and Var). *)

;

(* So that testing diff and simplify print out the full output. *)
Control.Print.printDepth := 100
