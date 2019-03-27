exception InvalidArgument

fun good_max (xs : int list) = 
    if null xs
      then 0 (* horrible style; fix later *)
    else if null (tl xs)
      then hd xs
    else 
        let val max_rest = good_max(tl xs)
        in
            if hd xs > max_rest
            then hd xs
            else max_rest
        end
 
fun good_max (xs, exc) = 
  case xs of
       [] => raise exc
     | x::[] => x
     | x::xs' => Int.max(x, good_max(xs'))


fun fibo (0) = 1
  | fibo (1) = 1
  | fibo (n) = fibo(n-1) + fibo(n-2)

fun fibo_series(0) = [fibo(0)]
  | fibo_series(n) = fibo_series(n-1) @ [fibo(n)]


(* fibo3 returns a tuple (f_n, f_(n-1))
   e.g. fibo3 0 ==> (1, 1)
        fibo3 1 ==> (2, 1)
        fibo3 2 ==> (3, 2)
 *)
fun fibo3 (0) = (1, 1)
  | fibo3 (n) =
  let val (p, pp) = fibo3(n-1) (*  n=1, fibo3(0) => (1,1) ==>  (2, 1) *)
  in
      (p+pp, p)
  end

(* nice example of nested pattern.  *)

(* nondecreasing: int list -> bool
 *
 * e.g.  [1,1,2, 3, 4, 5, 5] => true
 *       [1, 2, 1, 3, 5] => false
 *)
fun nondecreasing xs =
    case xs of
	[] => true
      | x::[] => true
      | head::(neck::rest) => (head <= neck andalso nondecreasing (neck::rest))

datatype sgn = P | N | Z

(* multsign: int * int -> sgn
 *
 * e.g. 1, 1 => P
 *      ~1, 10 => N
 *      ~10, ~42 => P
 *)
fun multsign (x1,x2) = 
  let fun sign x = if x=0 then Z else if x>0 then P else N 
  in
      case (sign x1,sign x2) of
	  (Z,_) => Z
	| (_,Z) => Z
	| (P,P) => P
	| (N,N) => P
	| _     => N (* many say bad style; I am okay with it *)
  end

