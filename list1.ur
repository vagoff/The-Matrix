
(* local *)

structure L0 = List0
con list0  = L0.list0
con count0 = Base.count0
open Compare

(* hidden *)
con list1 e = e * list0 e

(* local *)

fun convert [e ::: Type] ((x,xs) : list1 e) : list0 e = x :: xs

(* public *)

fun length [e] ((_,xs) : list1 e) : count0 = L0.length xs + 1

fun concat [e ::: Type] ((x,xs) : list1 e) (ys : list1 e) : list1 e = (x, L0.concat xs (convert ys))

fun rev [e ::: Type] (l1 : list1 e) (l2 : list1 e) : list1 e =
    case L0.rev (convert l1) (convert l2) of
	[] => (Tuple.fst l1, []) (* unreachable *)
      | x' :: xs' => (x', xs')

fun reverse [e ::: Type] ((e,l) : list1 e) =
    case L0.reverse (e :: l) of
	[] => (e, []) (* unreachable *)
      | x :: xs => (x, xs)

fun mp [a] [b] (f : a -> b) ((x,xs) : list1 a) : list1 b = (f x,L0.mp f xs)

fun max [e] (cmp : compare e) ((x,xs) : list1 e) : e =
    let
	fun max' m xs =
	    case xs of
		[] => m
	      | x :: xs' =>
	    	    case compare m x of
			LT => max' x xs'
		      |_ => max' m xs'
    in
	max' x xs
    end

fun foldl [e ::: Type] [s ::: Type] (f : e -> s -> s) (s : s) (l : list1 e) = L0.foldl f s (convert l)
fun foldr [e ::: Type] [s ::: Type] (f : e -> s -> s) (s : s) (l : list1 e) = L0.foldr f s (convert l)
fun foldli [e ::: Type] [s ::: Type] (f : int -> e -> s -> s) s (l : list1 e) = L0.foldli f s (convert l)
fun foldri [e ::: Type] [s ::: Type] (f : int -> e -> s -> s) s (l : list1 e) = L0.foldri f s (convert l)

fun repeat [t ::: Type] n (e : t) =
    if n > 0 then
        (e, L0.repeat (n - 1) e)
    else
	Base.error "Invalid argument n in List1.repeat"

fun repeati [t ::: Type] n (f : int -> t) =
    if n > 0 then
        (f 0, L0.repeati (n - 1) (fn i => f (i + 1)))
    else
	Base.error "Invalid argument n in List1.repeat"

fun mapX [e] (f : e -> xbody) (xs : list1 e) : xbody = L0.mapX f (convert xs)
fun foldMapX [e] [s] (f : e -> xbody) (st : s) (xs : list0 e) : xbody = L0.foldMapX f st (convert xs)
