(*
        Assignment1
        2015004575 JunHyeok Bae
*)

fun merge (xs:int list, ys:int list) = 
    if null xs orelse null ys (*check xs list or ys list is empty*)
    then
        let
        in
                if null xs andalso null ys (*if two lists are empty*)
                then [] (*return empty (ending merge)*)
                else 
                        let
                        in
                                if null xs (*case that only ys left*)
                                then hd ys::merge([],tl ys)
                                else hd xs::merge(tl xs,[]) (*case that only xs left*)
                        end                
        end
    else (*normal cases*)
        let
                val first_list_hd = hd xs
                val second_list_hd = hd ys
        in
                if first_list_hd < second_list_hd (*compare two list's head*)
                then first_list_hd::merge(tl xs,ys) (*Put smaller one*)
                else second_list_hd::merge(xs,tl ys) 
        end


fun reverse (xs:int list) =
    if null xs then []
    else
        let
                fun rever(xs:int list, tmp:int list) = (*tmp is the return value*)
                        if null xs then tmp (* if xs list is empty, end calc*)
                        else
                                rever(tl xs, hd xs::tmp) (* put head into tmp list*)
        in
                rever(xs, []) (*calc*)
        end


fun sigma (a:int, b:int, f:int->int) = 
    if a>b then 0 (* keep calc when a=b*)
    else sigma(a+1,b,f)+f(a) (* f(a)+f(a+1)+f(a+2)+...+f(b)*)


fun digits (a:int) =
    if a = 0 then []
    else 
        let
                fun sep(num:int, tmp:int list) = (*tmp is the return value*)
                        if num = 0 then tmp 
                        else
                                sep((num div 10), (num mod 10)::tmp) (* put a single digit into tmp list*)
        in
                sep(a,[]) (*calc*)
        end

fun additivePersistence (x:int) =
        let
                fun calc_digitroot(x:int, sum:int) = (*sum = plus each digits*)
                        if (x div 10) = 0 (*when x is a single digit*)
                        then sum+x
                        else calc_digitroot((x div 10),sum+(x mod 10)) (* calc each digits*)

                fun count_digitroot(x:int, cnt:int) = (*cnt++ when we calc to find digitroot*)
                        if (x div 10) = 0 (* when each digits's sum is a single digit*)
                        then cnt (* return persistence *)
                        else count_digitroot(calc_digitroot(x,0),cnt+1) (* count persistence and re_calc digitalroot*)
        in
                count_digitroot(x,0)
        end

fun digitalRoot (x:int) =
        let
                fun calc_digitroot(x:int, sum:int) = (*sum = plus each digits*)
                        if (x div 10) = 0 (*when x is a single digit*)
                        then sum+x
                        else calc_digitroot((x div 10),sum+(x mod 10)) (* calc each digits*)

                fun find_digitroot(x:int) = 
                        if (x div 10) = 0 (* when each digits's sum is a single digit => digitalRoot*)
                        then x (* return single digit => digitalRoot *)
                        else find_digitroot(calc_digitroot(x,0)) (* if x is not a single digit, re_calc digitalroot*)
        in
                find_digitroot(x)
        end




(*test

val testmerge = merge([4,5,6],[1,2,3])
val testreverse = reverse([1,5,4,3,2,3])
val testsigma = sigma(1,1,fn para=>para*para)
val testdigit = digits(2500)
val testPersistence = additivePersistence(12349)
val testdigitalRoot = digitalRoot(12349)

test*)
