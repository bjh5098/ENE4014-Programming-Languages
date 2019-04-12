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
		fun ck_strlist(pat,slist) =
			case pat of
				Variable(s) => slist @ [s]
			|	ConstructorP(s,p) => ck_strlist(p,slist)
			|	TupleP(plist) => foldl ck_strlist slist plist
			|	_ => slist

		fun isDistinct(slist) =
			case slist of
				[] => true
			|	x::[] => true
			|	x1::x2::xs => if(x1 = x2) then false
					      else true andalso isDistinct(xs)
	in
		if(isDistinct(mk_strlist(p,[]))) then true
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
