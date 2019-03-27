(* This is comments. (*This is neested comments *)*)

(*val x = 42;*)
(*
    static enviroment { x: int}
    dynamic enviroment { x: 42}
*)

(*val y = 30;*)
(*
    static enviroment {x:int, y:int}
    dynamic enviroment {x:42, y:30}
*)

(*val z = (x+y) * (x-y);*)
(*
    static enviroment {x:int, y:int, z:int}
    dynamic enviroment {x:42, y:30, z=XXX}
*)

(*val abs_of_z = if z>0 then z else 0-z;*)


val testlist = [1]
val testhd = hd testlist
val testtl = tl testlist

(*
    if z<0 then ~z else z
*)
(*

    syntax : if e1 then e2 else e3
    type check :    type check e1 ---> t1, t1 : bool
                    type check e2, e3 ---> t2, t3
                    whole expression --> t2, t3
    eval : evaluate e1 -> v1
            if v1 = true: eval e2 -> v2 ==> whole expression v2
            if v1 = false: eval e3 -> v3 ==> whole expression v3
*)
