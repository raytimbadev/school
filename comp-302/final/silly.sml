(* My Silly Language, by Jacob Thomas Erringtion *)


(* First some preliminary data structures that I need.
*******************************************************)

(* Used to represent things I haven't coded yet. *)
exception NotImplemented

(* A dictionary datatype built out of a linked list.
* This is super inefficient (O(n) lookup and insert) but who cares. n is small
*)
datatype ('k, 'a) Map = Map of ('k * 'a) list

(* Safely look up a key in the dictionary, returning an option value. *)
fun lookup k (Map []) = NONE
  | lookup k (Map ((k', v)::xs)) = if k = k' then v else lookup k (Map xs)

(* An exception to represent lookup failures in the dictionary. *)
exception NoKey

(* Unsafely look up a key in the dictionary, throwing NoKey if the key doesn't
* exist. *)
fun lookup' k (Map []) = raise NoKey
  | lookup' k (Map ((k', v)::xs)) = if k = k' then v else lookup' k (Map xs)

(* Insert a key into the dictionary. Keys must be unique. If an insert is made
* onto a key that is already in use, then the value at that key is simply
* updated. *)
fun insert k v (Map []) = Map [(k, v)]
  | insert k v (Map ((k', v')::xs)) = if k = k'
                                    then Map ((k, v)::xs)
                                    else let
                                           val Map ys = insert k v (Map xs)
                                         in
                                           Map ((k', v')::ys)
                                         end


(* Now I can implement the interpreter.
****************************************)

(* Identifiers in my silly language are simply strings. *)
type Identifier = string

(* There are 6 kinds of statements in my silly language.
* Each statement furthermore has a value. *)
datatype Statement = Print of Expression
                   (* Prints the value of the expression to the terminal. This
                   * will crash with a TypeError if the expression evaluates to
                   * a Function (or anything that's not a Natural, but so far
                   * there're only two types of data.
                   * The value of this statement is the value of the expression.
                   *)
                   | Copy of Identifier * Expression
                   (* Binds the identifier to the value of the expression.
                   * The value of this statement is the value of the expression *)
                   | Pred of Expression * Statement
                   (* *Tries* to take the predecessor of the given expression.
                   * The expression is evaluated; if it does not evaluate to a
                   * Natural, then a TypeError will crash the interpreter.
                   * Else, if the Natural is zero (Z), then we can't take its
                   * predecessor, so we execute the supplied statement.
                   * Otherwise, we take the predecessor. 
                   * If the predecessor can be taken, then this statement
                   * evaluates to the predecessor.
                   * Else, it evaluates to the value of the supplied statement.
                   * Note that the statement will not be executed if the
                   * predecessor can be taken !This is how we can do branching
                   * in my silly language. *)
                   | Expression of Expression
                   (* Expressions can be executed directly as statements. This
                   * is useful in particular for calling functions.
                   * This statement simply evaluates to the value of the
                   * expression. *)
                   | Return of Expression
                   (* Control returns to the calling environment and the value
                   * of the function becomes the value of the supplied
                   * expression. *)
                   | Block of Statement list
                   (* A list of statements can be executed as a single compound
                   * statement, in which case the value of block is the value of
                   * the last statement in the list.
                   * Note that an empty block will crash the interpreter with an
                   * EmptyBlock exception. *)

(* There are four kinds of expressions in my silly language. *)
and Expression = Literal of Value
               (* Literals are raw values. Simply yields the inner value. *)
               | Variable of Identifier
               (* Evaluates to the value of the variable, if it can be found in
               * the environment. Else the interpreter crashes with a NoKey
               * exception. *)
               | Succ of Expression
               (* Evaluates the given expression and then evaluates to its
               * successor. If the given expression does not evaluate to a
               * natural, then a TypeError will crash the interpreter. *)
               | Call of Expression
               (* Evaluates the given expression and then calls it. If the given
               * expression does not evaluate to a fucntion, then a TypeError
               * will crash the interpreter. *)

(* There are two kinds of values in my silly language. *)
and Value = Natural of Natural
          (* Natural numbers. *)
          | Function of Statement
          (* Functions.
          * Note that these aren't "real" functions at all, because there's no
          * local scope in my silly language. Everything is global.
          *)

(* Inductively defined natural numbers.
* Either a natural is zero (Z) or it is the successor of another natural (S n).
*)
and Natural = Z | S of Natural

(* Our symbol table is a dictionary from identifiers to values. *)
type SymbolTable = (Identifier, Value) Map

(* Convenience function for representing a natural as an integer. *)
fun irepr Z = 0
  | irepr (S n) = 1 + irepr n

(* The fatal errors in my silly language (plus the NoKey error when undefined
* variables are encountered. *)
exception TypeError of string
exception EmptyBlock
exception InvalidStatement

(* The nonfatal error used for implementing Return. *)
exception ReturnValue of Value * SymbolTable

(* evaluate : Expression
*           -> SymbolTable
*           -> (Value, Map Identifier Value)
*)
fun evaluate (Literal v) m = (v, m)
  | evaluate (Variable i) m = (lookup' i m, m)
  | evaluate (Succ e) m = 
  let
    val (v, m') = evaluate e m
  in
    case v
      of Natural n => let val v = Natural (S n) in (v, m) end
       | _ => raise (TypeError "expected natural")
  end
  | evaluate (Call e) m =
  let
    val (v, m') = evaluate e m
  in
    case v
      of Function body => execute body m' handle ReturnValue (v, m') => (v, m')
       | _ => raise (TypeError "expected statement")
  end

and execute (Print e) m =
  let
    val (v, m') = evaluate e m
  in
    case v
      of Natural n => (print (Int.toString (irepr n)) ; print "\n" ; (v, m'))
       | _ => raise (TypeError "expected int")
  end
  | execute (Copy (i, e)) m =
  let
    val (v, m') = evaluate e m
  in
    (v, insert i v m')
  end
  | execute (Pred (e, s)) m =
  let
    val (v, m') = evaluate e m
  in
    case v
      of Natural n =>
       (case n
          of Z => execute s m'
           | (S n') => (Natural n', insert i (Natural n') m')
       )
       | _ => raise (TypeError "expected natural")
  end
  | execute (Expression e) m = evaluate e m
  | execute (Block []) _ = raise EmptyBlock
  | execute (Block [s]) m = execute s m
  | execute (Block (s::ss)) m =
  let
    val (_, m') = execute s m
  in
    execute (Block ss) m'
  end
  | execute (Return e) m =
  let
    val (v, m') = evaluate e m
  in
    raise ReturnValue (v, m)
  end

(* Here is a sample program that defines two functions `add2` and `add`.
* I use the following convention for function arguments:
* take the name of the function, put an underscore, then put `x` (and maybe `y`)
* for unary (or binary) functions, or put `1`, then `2`, etc. for functions of
* more than one or two variables
* The program then uses add2 to store the value 4 in the variable "x" and the
* value 6 in the variable "y". Then it adds them together "x" and "y", storing
* the result in "sum". Finally it prints the sum.
*)
val sample = Block [
Copy ("add2",
     Literal (Function (Block [
     Copy ("add2_x", Succ (Variable "add2_x")),
     Copy ("add2_x", Succ (Variable "add2_x"))
     ]))),
Copy ("add",
     Literal (Function (Block [
     Pred ("add_x", (Return (Variable "add_y"))),
     Copy ("add_y", (Succ (Variable "add_y"))),
     Expression (Call (Variable "add"))
     ]))),

Copy ("add2_x", Variable "z"),
Expression (Call (Variable "add2")),
Expression (Call (Variable "add2")),
Copy ("x", Variable "add2_x"),

Copy ("add2_x", Variable "z"),
Expression (Call (Variable "add2")),
Expression (Call (Variable "add2")),
Expression (Call (Variable "add2")),
Copy ("y", Variable "add2_x"),

Copy ("add_x", Variable "x"),
Copy ("add_y", Variable "y"),
Copy ("sum", (Call (Variable "add"))),

(* Expected value in "sum" is 10. *)
Print (Variable "sum")
]

(* Here is the function that runs a program in my silly language. All programs
* in My Silly Language are given the variable `z` initialized to zero (Z). *)
fun run p = execute p (Map [("z", Natural Z)])
