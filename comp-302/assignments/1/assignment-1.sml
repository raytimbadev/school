(* Problem 1 *)
type date = int * int * int (* year-month-day *)

exception DateError

(* Problem 1.1 *)

(* TODO check that y >= 1 and use a case matching for all these patterns. *)
fun last_day (_, 1, _) = 31
  | last_day (y, 2, _) =
    if (y mod 400) = 0
    then 29
    else if (y mod 100) = 0
         then 28
         else if (y mod 4) = 0
              then 29
              else 28
  | last_day (_, 3, _) = 31
  | last_day (_, 4, _) = 30
  | last_day (_, 5, _) = 31
  | last_day (_, 6, _) = 30
  | last_day (_, 7, _) = 31
  | last_day (_, 8, _) = 31
  | last_day (_, 9, _) = 30
  | last_day (_, 10, _) = 31
  | last_day (_, 11, _) = 30
  | last_day (_, 12, _) = 31
  | last_day (_, _, _) = raise DateError

(* Problem 1.2 *)
fun valid_date (y, m, d) = m >= 1 
  andalso m <= 12 
  andalso d >= 1 
  andalso d <= last_day (y, m, d)

(* Problem 1.3 *)
fun next_day (y, 12, 31) = (y + 1, 1, 1)
  | next_day (y, m, d) = if last_day (y, m, d) = d 
                         then (y, m + 1, 1) 
                         else (y, m, d + 1)

(* Problem 1.4 *)
fun precedes (y1, m1, d1) (y2, m2, d2) = 
  if not (valid_date (y1, m1, d1) andalso valid_date (y2, m2, d2)) 
  then raise DateError
  else y1 < y2 orelse m1 < m2 orelse d1 < d2

(* Problem 1.5 *)
fun earliest_date xs = 
  let 
    fun min_date (d1, d2) = if precedes d1 d2 
                            then d1
                            else d2
  in
    foldl min_date (hd xs) xs
  end

(* Problem 2 *)
(* Problem 2.1 *)
fun zip nil _ = nil
  | zip _ nil = nil
  | zip (x::xs) (y::ys) = (x, y) :: zip xs ys

(* Problem 2.2 *)
(*
fun unzip ts = 
let
  fun unlink ((s, t), (ss, ts)) = (s::ss, t::ts)
in
  foldr unlink (nil, nil) ts
end
*)

fun unzip [] = ([], [])
  | unzip ((x1, x2)::xs) = 
  let
    val (ys1, ys2) = unzip xs
  in
    (x1::ys1, x2::ys2)
  end

(* Problem 2.3 
* Prove:
*   for each list lst, where lst is a list of tuples.
*   zip (unzip lst) = lst
* Base case, xs matches []
*   zip (unzip []) == zip ([], [])
*   # unzip [] is valuable because it's the base case of unzip
*   zip ([], []) == []
*   # zip ([], []) is valuable becase it matches one of the base case patterns
*   # of zip.
* Inductive step, lst matches x::xs
*   Inductive hypothesis: zip (unzip lst) == lst
*   # Replace lst with its match x::xs
*   zip (unzip (x::xs)) 
*   # Replace x with its match (x1, x2)
*   zip (x1::ys1, x2::ys2)
*   # partially evaluate zip
*   (x1, x2) :: zip ys1 ys2
*   # evaluate zip
*   (x1, x2) :: xs == lst
*)

(* Problem 2.4 *)

(* Due to truncation in the case of unequal lists in `zip`, data can be lost *)

(* Problem 3 *)

(* The incr function seen in class, which is really just a specialization of
 * `map` *)
fun incr ([], _) = []
  | incr (x::xs, c) = (x + c) :: incr (xs, c)

(* Problem 3.1 *)
fun prefixSum [] = []
  | prefixSum (x::xs) = x :: incr(prefixSum xs, x)

(* Problem 3.2 
* T(0) = 0
* T(n) = k(n - 1) + T(n - 1)     -- the recurrence
* T(n) = k(n - 1) + k(n - 2) + ... + k
* T(n) = k * sum (i from 1 to n-1) of (n - i)
* T(n) = k * sum (i from 1 to n-1) of (i)        -- (inversing the terms)
* T(n) = k * n(n - 1)/2
* Therefore, prefixSum is O(n^2) 
*)

(* Problem 3.3 *)
fun prefixSumFast xs = 
let 
  (* Define a helper function with an explicit accumulator parameter. *)
  fun go [] _ = []
    | go (x::xs) acc = (x + acc) :: go xs (x + acc)
in
  go xs 0
end

(* Problem 3.4 
* T(0) = 0
* T(n) = k + T(n - 1), where k is the cost of calculating (x + acc)
* T(n) = kn
* Therefore, prefixSumFast is O(n)
*)

