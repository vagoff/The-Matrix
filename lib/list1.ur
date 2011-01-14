(* [!] check for stack overflows *)

(* local *)

structure L0 = List0
con list0  = L0.list0
con count0 = Base.count0
open Compare

(* hidden *)
con list1 e = e * list0 e

(* local *)

fun convert [e] ((x,xs) : list1 e) : list0 e = x :: xs

(* public *)

fun length [e] ((_,xs) : list1 e) : count0 = L0.length xs + 1

fun concat [e] ((x,xs) : list1 e) (ys : list1 e) : list1 e = (x, L0.concat xs (convert ys))

fun rev [e] (l1 : list1 e) (l2 : list1 e) : list1 e =
    case L0.rev (convert l1) (convert l2) of
	[] => (Tuple.fst l1, []) (* unreachable *)
      | x' :: xs' => (x', xs')

fun reverse [e] ((e,l) : list1 e) =
    case L0.reverse (e :: l) of
	[] => (e, []) (* unreachable *)
      | x :: xs => (x, xs)

fun mp [a] [b] (f : a -> b) ((x,xs) : list1 a) : list1 b = (f x,L0.mp f xs)

fun flatten [e] (((x,xs),xxs) : list1 (list1 e)) : list1 e = (x, L0.concat xs (L0.flatten (mp convert xxs)))

fun foldl [e] [s] (f : e -> s -> s) (s : s) (l : list1 e) = L0.foldl f s (convert l)
fun foldr [e] [s] (f : e -> s -> s) (s : s) (l : list1 e) = L0.foldr f s (convert l)
fun foldli [e] [s] (f : int -> e -> s -> s) s (l : list1 e) = L0.foldli f s (convert l)
fun foldri [e] [s] (f : int -> e -> s -> s) s (l : list1 e) = L0.foldri f s (convert l)

fun max [e] (cmp : compare e) ((x,xs) : list1 e) : e = L0.max x xs
fun min [e] (cmp : compare e) ((x,xs) : list1 e) : e = L0.min x xs

fun repeat [t] n (e : t) =
    if n > 0 then
        (e, L0.repeat (n - 1) e)
    else
	    Base.error "Invalid argument n in List1.repeat"

fun repeati [t] n (f : int -> t) =
    if n > 0 then
        (f 0, L0.repeati (n - 1) (fn i => f (i + 1)))
    else
    	Base.error "Invalid argument n in List1.repeat"

fun mapX [e] (f : e -> xbody) (xs : list1 e) : xbody = L0.mapX f (convert xs)
fun foldMapX [e] [s] (f : e -> xbody) (s : s) (xs : list0 e) : xbody * s = L0.foldMapX f s (convert xs)
fun depMapX [e] [s] (f : e -> xbody) (s : s) (xs : list0 e) : xbody = L0.depMapX f s (convert xs)

fun mapM [e] [m] (m : monad m) (f : e -> m r) ((x,xs) : list0 e) : m (list r) =
    x' <- f x;
    xs' <- L0.mapM f xs;
    return (x',xs')

fun mapMX [e] [m] (m : monad m) (f : e -> m xbody) ((x,xs) : list1 e) : m xbody =
    return <xml>{f x}{L0.mapMX f xs}</xml>
