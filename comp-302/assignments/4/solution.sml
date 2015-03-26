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

(*
* To find the value of f(y), we just follow the arrows whenever we need to look
* up a symbol.
* We find that the value of f(y) is 6.
*)

;

(* So that testing diff and simplify print out the full output. *)
Control.Print.printDepth := 100
