(* abstract data type using closures *)

(* int set 
 * with 3 methods: insert, member, size
 *                 notice insert returns a set (a new set)
 *)

datatype set = S of { insert : int -> set, 
		              member : int -> bool, 
		              size   : unit -> int,
		              members: int list,
                              delete : int -> set
		              }

(* implementation of sets: this is the fancy stuff, but clients using
   this abstraction do not need to understand it *)
val empty_set =
    let
        fun make_set xs = 
            let fun contains i = List.exists (fn j => i=j) xs
            in
                S { insert = fn i => if contains i 
                                     then make_set xs 
                                     else make_set (i::xs),
                    member = contains,
                    size   = fn () => length xs,
                    members = xs,
                    delete = fn i => make_set (List.filter (fn j => i<>j) xs)
                }
            end
    in
        make_set []
    end 

(* example client *)
val S s1 = empty_set;
val S s2 = (#insert s1) 34;
val S s3 = (#insert s2) 34;
val S s4 = #insert s3 19;
fun map (f, xs) =
  case xs of
      [] => []
    | x::xs' => f(x)::map(f, xs')

fun filter(f, xs) = 
  case xs of
    [] => [] 
  | x::xs' => if f x then x::filter(f, xs') 
                     else filter(f, xs')

fun fold(f, acc, xs) = 
    case xs of 
      []     => acc
    | x::xs' => fold(f, (f(acc,x)), xs')

val nums_list = [[9, 40, 75, 7],
                 [64, 34, 88, 96],
                 [91, 92, 53, 31],
                 [50, 84, 73, 65],
                 [54, 44, 75, 11],
                 [91, 71, 48, 46],
                 [70, 72, 5, 42],
                 [25, 77, 49, 56],
                 [89, 4, 73, 52],
                 [36, 56, 61, 1]]
(* fun fold(f, acc, xs) *)
val local_max = map(fn nums => fold(Int.max, hd(nums), nums),
                    nums_list)
(* [75, 96, ... ] *)

val global_max = fold(Int.max, hd(local_max), local_max)

(* given x, count the multiples of x in each list
 * x=11, [[1, 2, 11], [2, 3, 22, 33], [4, 5]]
 * ==>   [1, 2, 0]
 *)
(* 1. apply modular (filter) to *each* list
 * ==> [[11], [22, 33], []]
 * 2. count *each* list
 * ==> [1, 2, 0]
 *)
(* filter(f, xs) *)


fun count_multiples (x, nums_list) =
    map(length,
        map(fn nums => filter(fn y => (y mod x)=0 , nums),
            nums_list))





(* similar to above, given x, count the multiples of x in each list
 * and returns the index of the list having the maximum count.
 * x=11, [[1, 2, 11], [11, 22, 33], [4, 5]]
 * ==>   1  
 *)

(* 1. call count_multiples above
 * 2. apply fold. acc = (max_index, curr_index, max_value)
 *)
fun index_of_max_multiple_count (x, nums_list) =
let val counts = count_multiples(x, nums_list)
(*   [1, 3, 3] *)
in
    fold(fn (acc, y) => if (#3 acc) > y then (#1 acc, 1+(#2 acc), #3 acc)
                                        else (#2 acc, 1+(#2 acc), y),
        (0, 0, hd(counts)),
        counts)
end
