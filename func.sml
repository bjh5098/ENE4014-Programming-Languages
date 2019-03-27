(* compute the square of x *)
fun square (x:int) = 
    x*x

(* compute x to the square of y *) (* y>=0 *)
fun pow (x:int, y:int) =
    if y=0 then 1
    else x*pow(x,y-1)

(* compute the cube of x *)
fun cube (x: int)=
    pow(x,3)

val eight = cube 3

val very_large_number = pow(3,10)