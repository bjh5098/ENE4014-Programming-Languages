fun apply_f(f:int*)

(*pairs*)
fun sort_pair (pr:int * int)
    if #1 pr < #2 pr
    then pr
    else swap (pr)

(*list*)

fun sum_list (xs : int list)
    if null xs
    then 0
    else hd(xs) + sum_list(tl(xs))


fun countdown (x : int) =
    if x=0
    then []
    else x :: countdown (x-1)

fun append (xs : int list, ys : int list) =
    if null xs
    then ys
    else hd (xs) :: append (tl(xs), ys)


(*3/13 Wed*)

fun better_max (xs : int list) = 
    if null xs
        then NONE
    else
        let val max_rest = better_max(tl xs)
        in
            if isSome(max_rest) 
                andalso valOf(max_rest) > hd xs
            then max_rest
            else SOME (*NONE*)
        end
(*NONE*)

fun better_max2 (xs : int list) = 
    if null xs
        then NONE
    else
        let val max_nonempty(xs: int list) =
        if null (tl sx)
        then hd xs
        else
            let val max_rest = max_nonempty(tl xs)
            in
                if hd xs > max_rest
                then hd xs
                else max_rest
            end
        
        in
            SOME max_nonempty(xs)
        end

(*NONE*)