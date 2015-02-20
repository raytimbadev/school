fun length nil = 0
  | length (x::xs) = 1 + length xs

fun lengthTR xs =
let
  fun go nil n = n
    | go (x::xs) n = go xs (n + 1)
in
  go xs 0
end

fun reverse xs =
let
  fun go nil ys = ys
    | go (x::xs) ys = go xs (x::ys)
in
  go xs []
end

fun map f nil = nil
  | map f (x::xs) = f x :: map f xs

fun mapTR f xs = 
let
  fun go nil ys = ys
    | go (x::xs) ys = go xs (f x :: ys)
in
  reverse (go xs nil)
end

fun foldr _ [] t = t
  | foldr f (x::xs) t = f (foldr f xs t) x

fun foldl f xs t =
let
  fun go acc [] = acc
    | go acc (x::xs) = go (f acc x) xs
in
  go t xs
end

fun fibonacciTR n = 
let
  fun go _  f  0 = f
    | go f1 f2 m = go f2 (f1 + f2) (m - 1)
in
  go 1 0 n (* order matters! *)
end

datatype Mathexp = Num of int
                 | Var of string
                 | Neg of Mathexp
                 | Add of Mathexp * Mathexp
                 | Mul of Mathexp * Mathexp

datatype 'a myList = myNil
                   | myCons of 'a * 'a myList

datatype 'a myOption = myNone
                     | mySome of 'a

datatype 'a tree = Empty 
                 | Node of 'a * 'a tree * 'a tree

datatype ('a, 'b) myEither = myLeft of 'a
                        | myRight of 'b
                      
