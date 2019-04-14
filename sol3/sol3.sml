(*
        Assignment3
        2015004575 JunHyeok Bae 
*)

(*Problems 1*)
datatype pattern = Wildcard 
                | Variable of string 
                | UnitP
                | ConstP of int 
                | TupleP of pattern list
                | ConstructorP of string * pattern
datatype valu = Const of int 
                | Unit 
                | Tuple of valu list
                | Constructor of string * valu                

fun check_pat(p) = 
	let
		fun ch_strlist(pat,slist) =
			case pat of
				Variable(s) => slist @ [s]
			|	ConstructorP(s,p) => ch_strlist(p,slist)
			|	TupleP(plist) => foldl ch_strlist slist plist
			|	_ => slist
 
		fun isDistinct(slist) =
			case slist of
				[] => true
			|	x::[] => true
			|	x1::x2::xs => if(x1 = x2) 
                                                then false
					        else true andalso isDistinct(xs)
	in
		if(isDistinct(ch_strlist(p,[]))) 
                then true
		else false
	end

(*Problems 2*)
fun match(v,p) = 
	let 

	fun plus_match(x) = 
		case x of
			[] => []
		|	x::xs => if(match(x) = NONE) 
                                then plus_match(xs) 
                                else valOf(match(x)) @ plus_match(xs)

	fun check_match(x) = 
		case x of
			[] => true
		|	x::xs => if(match(x) = NONE) 
                                then false
				else check_match(xs)

	in
(*
datatype pattern = Wildcard 
                | Variable of string 
                | UnitP
                | ConstP of int 
                | TupleP of pattern list
                | ConstructorP of string * pattern
datatype valu = Const of int 
                | Unit 
                | Tuple of valu list
                | Constructor of string * valu
*)      
	case (v,p) of
		(_,Wildcard) => SOME []
	|	(_,Variable(s)) => SOME [(s,v)]
	|	(Unit,UnitP) => SOME []
	|	(Const(numv),ConstP(nump)) => if(numv = nump) 
                                        then SOME [] 
                                        else NONE	 
	|	(Constructor(s1,v0),ConstructorP(s2,p0)) => if(s1 = s2) 
                                                        then match(v0,p0) 
                                                        else NONE
	|	(Tuple(vs),TupleP(ps)) => if((List.length(vs) = List.length(ps)) andalso check_match(ListPair.zip(vs,ps))) 
									then SOME(plus_match(ListPair.zip(vs,ps)))
									else NONE
	|	_ => NONE
	
	end

(*Problems 3*)

type name = string

datatype RSP = ROCK 
			| SCISSORS 
			| PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament = PLAYER of name * (RSP strategy ref)
    				| MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))

fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two,one))

fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two,three,one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK,PAPER)
val sr = alterTwo(SCISSORS,ROCK)
val ps = alterTwo(PAPER,SCISSORS)
val rs = alterTwo(ROCK,SCISSORS)
val sp = alterTwo(SCISSORS,PAPER)
val pr = alterTwo(PAPER,ROCK)
val srp = alterThree(SCISSORS,ROCK,PAPER)
val spr = alterThree(SCISSORS,PAPER,ROCK)
val rsp = alterThree(ROCK,SCISSORS,PAPER)
val rps = alterThree(ROCK,PAPER,SCISSORS)
val prs = alterThree(PAPER,ROCK,SCISSORS)
val psr = alterThree(PAPER,SCISSORS,ROCK)


fun next(strategyRef) =
	let 
		val Cons(rsp, func) = !strategyRef
	in
		(strategyRef := func();rsp)
	end


fun whosWinner(t) = 
	let
	
	fun match_game(p_t1,p_t2) =
		case (p_t1,p_t2) of
			(PLAYER(a,b),MATCH(t1,t2)) => match_game(PLAYER(a,b), match_game(t1,t2))
		|	(MATCH(t1,t2),PLAYER(a,b)) => match_game(PLAYER(a,b), match_game(t1,t2))
		|	(MATCH(t1,t2),MATCH(t3,t4)) => match_game(match_game(t1,t2), match_game(t3,t4))
		|	(PLAYER(n1,strRef1),PLAYER(n2,strRef2)) => 
				let 
					val s1 = next(strRef1)
					val s2 = next(strRef2)
				in
					if(s1 = s2) then match_game(PLAYER(n1,strRef1),PLAYER(n2,strRef2))
					else if((s1 = ROCK andalso s2 = SCISSORS) orelse (s1 = SCISSORS andalso s2 = PAPER) orelse (s1 = PAPER andalso s2 = ROCK))
					then PLAYER(n1,strRef1)
					else PLAYER(n2,strRef2)
				end
	
	in

	case t of
		MATCH(t1,t2) => match_game(t1,t2)
					| _ => t
	end



(*test case*)
(*

val SampleConstructorP = ConstructorP ("ConName",TupleP([Variable "test3",Variable "test4"]))
val SampleConstructor = Constructor("ConName",Tuple ([Constructor("forTest3",Const 9), Tuple([Constructor("forTest4inTuple",Const 11 )])]))

val SamplePattern = TupleP([UnitP, ConstP 3, Variable "test1", Variable "test2", SampleConstructorP])
val SampleValu = Tuple([Unit,Const 3, Const 5, Const 6, SampleConstructor])
val endResult = check_pat(SamplePattern)

val testRSP = whosWinner(MATCH(PLAYER("s", ref s),MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r))))
val testRSP2 = whosWinner(MATCH(MATCH(PLAYER("rsp", ref rsp),PLAYER("ps",ref ps)),MATCH(PLAYER("srp", ref srp), PLAYER("prs", ref prs))))
*)
(*
        round 1 : rsp vs ps
        round 2 : srp vs prs
        round1 winner vs round 2 winner

        "srp" wins
*)