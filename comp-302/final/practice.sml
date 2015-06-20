datatype 'a MyList = Cons of 'a * 'a MyList | Nil
datatype 'a Stream = SCons of 'a * (unit -> 'a Stream)

val naturals =
let
  fun next n = SCons (n, fn () => next (n+1))
in
  next 0
end

fun stake 0 _ = []
  | stake k (SCons (n, next)) = n :: stake (k-1) (next ())

fun concat l = foldr (op @) [] l

fun intersperse y = tl o foldr (fn (x, ys) => y :: x :: ys) []

fun badIntersperse y = tl o concat o map (fn x => [y,x])

fun intercalate lst = concat o intersperse lst
