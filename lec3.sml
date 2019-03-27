(*record w/ different compound types*)
val x = {bar=("42", 42), foo=if true then 42 else 43, baz=(true, "43")}

(*record w/ name and age*)
val my_first_kid = {name:"Alex", age:10}
val my_second_kid = {}

(*val x = {1="42", 2=(true*)

datatype mytype = TwoInts of int*int
                | Str of string
                | Pizza


datatype suit = Club | Diamond | Heart | Space
datatype card_value = Jack | Queen | King | Ace | Num of int
datatype card = Card of suit * card_value

val hands = [Card(Club, Jack), Card(Club, Num(10)] of list

(*assume hand is not empty
returns true if suits of all cards are same
ex) Club 10 / Club King / Club Ace => True
Club 10 / Diamond King => false 
datatype card = card of suit * card value*)

fun is_flush (hand : card list) = 
    case hand of
        [] => true
        |_::[] => true (*_:: => 언더바에 뭐가 들어와도 상관없음.*)
        |Card(sh1, _)::Card(sh2, _)::[] => sh1 = sh2
        |Card(sh1, _)::Card(sh2, _)::rest => sh1 = sh2 andalso is_flush(tl hand)

(*블랙잭 : 21을 넘지 않으면서 21에 가깝게 가는 게임. 넘어가면 터짐*)

fun hasAce (hand : card list) =
    case hand of 
        [] => false
        |Card(_, Ace)::_ => true
        |_::rest => hasAce(rest)

fun removeAce (hand : card list) = 
    case hand of
        [] => []
        |Card(_, Ace)::rest => rest
        |h1::rest => h1::removeAce(tl rest)
(*
fun blackjack(hand : card list) = 
let val sum1 = simpleSum(hand)
in
    if sum1 < 21
    then sum1
    else if hasAce(hand)
        then 1+blackjack(removeAce hand)
        else sum1
end
*)
fun blackjack(score:int, hand : card list) =
    case hand of 
        [] => score
        |Card(_,Ace) => let val try1 = blackjack(score+11, rest)
                        in
                            if try1>21
                            then blackjack(score+1, rest)
                            else try1
                        end
        |Card(_,Num(i))::rest => blackjack(score+i, rest)
        |Card(_, _)::rest => blackjack(score+10, rest)
