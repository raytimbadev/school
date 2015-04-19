(*
exception Positive
exception Negative
exception Zero

fun wat 0 = raise Zero
  | wat n = if n > 0
            then raise Positive
            else raise Negative

fun sillyHandle n =
  wat n handle Zero => "zero"
             | Positive => "+"
             | Negative => "-"
*)

exception NotImplemented

datatype ('k, 'a) Map = Map of ('k * 'a) list

fun lookup k (Map []) = NONE
  | lookup k (Map ((k', v)::xs)) = if k = k' then v else lookup k (Map xs)

exception NoKey

fun lookup' k (Map []) = raise NoKey
  | lookup' k (Map ((k', v)::xs)) = if k = k' then v else lookup' k (Map xs)

fun insert k v (Map []) = Map [(k, v)]
  | insert k v (Map ((k', v')::xs)) = if k = k'
                                    then Map ((k, v)::xs)
                                    else let
                                           val Map ys = insert k v (Map xs)
                                         in
                                           Map ((k', v')::ys)
                                         end


type Identifier = string

datatype Statement = Print of Expression
                   | Copy of Identifier * Identifier
                   | Pred of Identifier * Statement
                   | Expression of Expression
                   | Return of Expression
                   | Block of Statement list

and Expression = Literal of Value
               | Variable of Identifier
               | Succ of Identifier
               | Call of Expression

and Value = Natural of Natural
          | Function of Statement

(* Inductively defined natural numbers. *)
and Natural = Z | S of Natural

(* Represents a natural as an integer. *)
fun irepr Z = 0
  | irepr (S n) = 1 + irepr n

exception TypeError of string
exception EmptyBlock
exception InvalidStatement

(* evaluate : Expression
*           -> Map Identifier Value
*           -> (Value, Map Identifier Value)
*)
fun evaluate (Literal v) m = (v, m)
  | evaluate (Variable i) m = (lookup' i m, m)
  | evaluate (Succ i) m = (
  case lookup' i m
    of Natural n => let val v = Natural (S n) in (v, insert i v m) end
     | _ => raise (TypeError "expected natural")
  )
  | evaluate (Call e) m = 
  let
    val (v, m') = evaluate e m
  in
    case v
      of Function body => execute body m'
       | _ => raise (TypeError "expected statement")
  end

and execute (Print e) m =
  let
    val (v, m') = evaluate e m
  in
    case v
      of Natural n => (print (Int.toString (irepr n)) ; (v, m'))
       | _ => raise (TypeError "expected int")
  end
  | execute (Copy (i1, i2)) m =
  let
    val v = lookup' i2 m
  in
    (v, insert i1 v m)
  end
  | execute (Pred (i, s)) m =
  let
    val v = lookup' i m
  in
    case lookup' i m
      of Natural n =>
       (case n
          of Z => execute s m
           | (S n') => (Natural n', insert i (Natural n') m)
       )
       | _ => raise (TypeError "expected natural")
  end
  | execute (Expression e) m = evaluate e m
  | execute (Block []) _ = raise EmptyBlock
  | execute (Block ((Return e)::_)) m = evaluate e m
  | execute (Block [s]) m = execute s m
  | execute (Block (s::ss)) m =
  let
    val (_, m') = execute s m
  in
    execute (Block ss) m
  end
  | execute (Return _) _ = raise InvalidStatement
