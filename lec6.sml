(*f is a normal function
f(x,y)
*)

fun curry f x y =
    f(x,y)

fun uncurry f (x,y) =
    f x y

(*change the argument order
*f x y  => f y x*)
fun other_curry1  f x y = f y x

fun other_curry2 
    fn x => fn y => f y x

(* example *)
(*tupled but we wish it were curried*)

fun range(i,j) = if i > j then else i :: range(i+1,j)

(*function counting form 1*)
val count_upto = curry range 1

(*xx = [1,2,3...7]*)
val xs = count_upto 7

(*count10from 7 ==> [7,8,9,10]*)

val count10from = other_curry2 (curry range) 10