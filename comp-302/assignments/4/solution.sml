(* Problem 1 *)

datatype 'a rlist = Empty | RCons of 'a * (('a rlist) ref)

(*
* Given a comparison function that defines the sorting order of a reference
* list, insert an element into the reference list such that if it was ordered
* according to the comparison function prior to insert, it will be after the
* insert operation as well, i.e. the element is inserted in order.
*
* The procedure below essentially traverses the reference list up to the point
* where the comparison y ~ x fails, where y denotes an element from the list and
* x denotes the element to insert. At that point, it performs the insert
* in-place, destructively updating the list.
*)
fun insert (cmp : 'a * 'a -> bool) (x : 'a) (xs : ('a rlist) ref) : unit =
  case !xs
    of Empty => xs := RCons (x, ref Empty)
     | RCons (y, ys) =>
         if not (cmp (y, x))
         then xs :=  RCons (x, ref (RCons (y, ys)))
         else insert cmp x ys

val testList = ref (RCons (1, ref (RCons (2, ref (RCons (3, ref (RCons (5, ref
Empty))))))))

(* Problem 2 *)

datatype transactions = Withdraw of int | Deposit of int | Check_balance

fun make_protected_account (initialBalance, password) =
  fn (t, passwordAttempt) =>
    if passwordAttempt = password
    then
      case t
        of Withdraw x => initialBalance - x
         | Deposit x => initialBalance + x
         | Check_balance => initialBalance
    else (print "Wrong password.\n"; 0)

val my_protected_account = make_protected_account (100, "secret")

(* Problem 3 *)

(* See the attached image for the diagram. *)

(* Here are the steps of the evaluation. *)

(* The innermost expression is `f(y)`, so its value is the overall value of the
* expression.
*
* Since SML is a strict language, we must evaluate `y` first.
*
* Lookup `y` in E1:
*   environment E2 is the first parent of E1 binding `y`, and y = 4 in E2,
*   so y = 4 in E1.
*
* Lookup `f` in E1:
*   environment E4 is the first parent of E1 binding `f`, and f is a function of
*   one symbol `u`.
*
* Evaluate f(y):
*   The symbol `u` bound by `f` is substituted for the value of `y` which we
*   found to be 4.
*   f(4) = 4 + x + y
*
*   Lookup `x` in E3:
*     `x` is bound to `y` in E3.
*     Lookup `y` in E3:
*       environment E5 is the first parent of E3 binding `y`, and y = 1 in E5.
*     Therefore, x = 1 in E3.
*   Lookup `y` in E3:
*     environment E5 is the first parent of E3 binding `y`, and y = 1 in E5.
*     Therefore, y = 1 in E3.
*
* Therefore f(4) = 4 + 1 + 1 = 6 is the final result of the evaluation.
*)

;

(* So that testing diff and simplify print out the full output. *)
Control.Print.printDepth := 100
