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

(* Problem 4 *)

(*
* Proposition. Let reduce and reduce_tr be be defined by the following
* equations.
*
* reduce nil base op = base
* reduce (x::xs) base op = op x ( reduce xs base op )
*
* reduce_tr nil base op = base
* reduce_tr (x::xs) base op = reduce_tr xs (op x base) op
*
* where op satisfies the following properties.
*
* op base m = m                     (base is the identity of op)
* op n m = op m n                   (op is commutative)
* op n (op m k) = op (op n m) k     (op is associative)
*
* Then,
*
* reduce l n op = reduce_tr l n op
*
* i.e. they compute the same thing.
*
* Lemma. For any h, l, and n, we have
*
* op h ( reduce l n op ) = reduce_tr l ( op h n ) op
*
* Proof of Lemma. By induction on the length of l.
*
* Base case: l is nil.
*
* LHS = op h ( reduce nil n op ) = op h n       (definition of reduce)
* RHS = reduce_tr nil ( op h n ) op = op h n    (definitino of reduce_tr)
*
* The base case is verified.
*
* Inductive hypothesis:
* op h ( reduce xs n op ) = reduce_tr xs ( op h n ) op
*
* Inductive step:
* op h (reduce (x::xs) n op)
* = op h (op x (reduce xs n op))            (definition of reduce)
* = op (op h x) (reduce xs n op)            (associativity of op)
* = reduce_tr xs (op (op h x) n) op         (induction hypothesis)
* = reduce_tr (x::xs) (op h n) op           (definition of reduce_tr)
*
* The inductive step is verfied.
* Proof of Lemma complete.
*
* Proof of Proposition.
*
* reduce l base op
* = op base (reduce l base op)              (base is identity of op)
* = reduce_tr l (op base base) op           (by Lemma)
* = reduce_tr l base op                     (base is identity of op)
*
* Thus, the programs reduce and reduce_tr are equivalent.
*
* End of proof.
*)

;

(* So that testing diff and simplify print out the full output. *)
Control.Print.printDepth := 100
