(* Assignment #2
 * By: Jacob Errington (260636023)
 *)

(* Problem 1 *)

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
  val h = 0.000001
  fun derivative f x = (f (x + h) - f x) / h

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
  newtonHelper f x0 e 1000
end
