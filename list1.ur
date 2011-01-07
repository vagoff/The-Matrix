structure List1 = struct

(* local *)

(* [!] bug in Ur/Web! *)
(* con list0  = List0.list0 *)
con list0 = list
con list1 e = e * list0 e

(* local *)

fun convert [e ::: Type] ((x,xs) : list1 e) : list0 e = x :: xs

(* public *)

fun concat [e ::: Type] ((x,xs) : list1 e) (ys : list1 e) : list1 e = (x, List0.concat xs (convert ys))

fun rev [e ::: Type] (l1 : list1 e) (l2 : list1 e) : list1 e =
    case List0.rev (convert l1) (convert l2) of
	[] => (Tuple.fst l1, []) (* unreachable *)
      | x' :: xs' => (x', xs')

fun reverse [e ::: Type] ((e,l) : list1 e) =
    case List0.reverse (e :: l) of
	[] => (e, []) (* unreachable *)
      | x :: xs => (x, xs)

fun foldl [e ::: Type] [s ::: Type] (f : e -> s -> s) (s : s) (l : list1 e) = List0.foldl f s (convert l)
fun foldr [e ::: Type] [s ::: Type] (f : e -> s -> s) (s : s) (l : list1 e) = List0.foldr f s (convert l)
fun foldli [e ::: Type] [s ::: Type] (f : int -> e -> s -> s) s (l : list1 e) = List0.foldli f s (convert l)
fun foldri [e ::: Type] [s ::: Type] (f : int -> e -> s -> s) s (l : list1 e) = List0.foldri f s (convert l)

fun repeat [t ::: Type] n (e : t) =
    if n > 0 then
        (e, List0.repeat (n - 1) e)
    else
	Base.error "Invalid argument n in List1.repeat"

fun repeati [t ::: Type] n (f : int -> t) =
    if n > 0 then
        (f 0, List0.repeati (n - 1) (fn i => f (i + 1)))
    else
	Base.error "Invalid argument n in List1.repeat"

(*
fun foldlmap f l s =
    let
	fun process ls =
	    case ls of
		x :: xs => f x s :: process
		*)

end
