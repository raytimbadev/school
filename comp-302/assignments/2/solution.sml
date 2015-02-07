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
