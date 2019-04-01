(*
        Assignment2
        2015004575 JunHyeok Bae 
*)

(*Problems 1*)
datatype expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr

datatype formula = TRUE
            | FALSE
            | NOT of formula
            | ANDALSO of formula * formula
            | ORELSE of formula * formula
            | IMPLY of formula * formula
            | LESS of expr * expr

fun exp (e:expr) =
    case e of
        NUM i => i
        | PLUS (e1,e2) => (exp e1) + (exp e2)
        | MINUS (e1,e2) => (exp e1) - (exp e2)

fun eval (x:formula) =
    case x of
        TRUE => true
        | FALSE => false
        | NOT x1 => not (eval x1)
        | ANDALSO(x1,x2) => (eval x1) andalso (eval x2)
        | ORELSE(x1,x2) => (eval x1) orelse (eval x2)
        | IMPLY(x1,x2) => if(not (eval x1))
                            then true
                            else (eval x2)
        | LESS(x1,x2) => (exp x1) < (exp x2)



(*Problems 2*)
type name = string
datatype metro = STATION of name
                | AREA of name * metro
                | CONNECT of metro * metro

fun checkMetro (x:metro) =
    let
        fun train(t) =
            case t of
                STATION n => (n::[])
                | AREA(n,x1) => let 
                                    val rest = train(x1)
                                in
                                    let fun checkExtence(xs) =
                                        if (null xs) then []
                                        else 
                                            let 
                                                val rest2 = checkExtence(tl xs)
                                            in
                                                if (hd xs) = n then rest2
                                                else (hd xs)::rest2
                                            end
                                    in
                                        checkExtence(rest)
                                    end
                                end
                | CONNECT(x1,x2) => (train(x1)) @ (train(x2))
    in
        let
            val result = train(x)
        in
            if (null result) 
            then true
            else false
        end
    end
        
(*Problems 3-1*)
    
datatype 'a lazyList = nullList
                    | cons of 'a * (unit -> 'a lazyList)    

fun seq(first,last) =
	if first = last then cons(first, fn() => nullList)
	else cons(first, fn() => seq(first+1,last))

fun infSeq(first) = 
    cons(first, fn() => infSeq(first + 1))

fun firstN(lazyListVal,n) =
    case (lazyListVal,n) of
        (nullList,_) => []
        | (_, 0) => []
        | (cons(hd,f),n1) => hd::firstN(f(),n1-1)

fun Nth(lazyListVal,n) =
    case (lazyListVal,n) of
        (nullList,_) => NONE
        | (cons(hd,f),1) => SOME hd
        | (cons(hd,f),n1) => Nth(f(),n1-1)

fun filterMultiples(lazyListVal,n) =
    case (lazyListVal,n) of
        (nullList,_) => nullList
        | (cons(hd,f),n) => if (hd mod n)=0
                            then filterMultiples(f(),n)
                            else cons(hd,fn() => filterMultiples(f(),n))


(*Problems 3-2*) 

fun primes() =
    let
        fun sieve(lazyListVal) =
        case (lazyListVal) of
            cons(hd,f) => cons(hd,fn() => sieve(filterMultiples(f(),hd)))
            |nullList => nullList
    in
        sieve(infSeq(2))
    end

(*
        test

val evalTest = eval(ANDALSO(ORELSE(ANDALSO(TRUE,FALSE),IMPLY(FALSE,FALSE)),LESS(PLUS(NUM(1),NUM(3)),(MINUS(NUM(5),NUM(2))))))

val mustTrue1 =checkMetro(AREA("a", STATION "a"))
val mustTrue2 =checkMetro(AREA("a", AREA("a", STATION "a")))
val mustTrue3 =checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) 
val mustTrue4 =checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))

val mustFalse1 =checkMetro(AREA("a", STATION "b"))
val mustFalse2 =checkMetro(AREA("a", AREA("a", STATION "b")))
val mustFalse3 =checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))
val mustFalse4 =checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))
val mustFalse5 =checkMetro(AREA("a", AREA("k", CONNECT(STATION "c", AREA("c",STATION("k"))))))


val testsq = seq(1,6)
val testmulti1 = firstN(filterMultiples(seq(2,6),2),10)
val testmulti2 = firstN(filterMultiples(seq(3,8),3),10)
val testprimes1 = firstN(primes(),20);
val testprimes2 = firstN(primes(),10);
val testprimes3 = Nth(primes(),20);

*)