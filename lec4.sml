fun nondecreasing xs =


data sgn 
fun multsign (x1:int, x2:int) =
    
    let fun sign x =
        if x=0
        then Z
        else if x>0
            then P
            else N
    in
        case (sign(x1), sign(x2)) of
            (P, P) => P
            | (N, N) => P
            | (Z, _) => Z
            | (_, Z) => Z
            | _ => N
    end

(*길이재기*)
fun len (l: 'a list) = 
    case l of
        [] => 0
        | _::xs => 1+len(xs) 

(*exception을 활용한 MAX구하기
호스트코드가 어떤 exception코드를 정할지
클라이언트 코드가 뭔가 문제가 생겼을때 대부분 에러메시지 printout?
exception 핸들링으로 쓸데없는 에러*)
exception InvalidArgument
fun max2 (xs: int list) =
    case xs of
            [] => raise InvalidArgument
        | x :: xs' => Int.max(x, max2(xs'))