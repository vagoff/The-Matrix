structure List0 = struct

con list0 e = list e

fun rev [e ::: Type] (xs : list0 e) (ys : list0 e) : list0 e =
    case xs of
	[] => ys
      | x :: xs => rev xs (x :: ys)

fun reverse [e ::: Type] (l : list0 e) = rev l []

fun foldl [e ::: Type] [s ::: Type] (f : e -> s -> s) (s : s) (l : list0 e) =
    let
	fun foldl' s xs =
	    case xs of
		[] => s
	      | x :: xs => foldl' (f x s) xs
    in
	foldl' s l
    end

fun foldr [e ::: Type] [s ::: Type] (f : e -> s -> s) (s : s) (l : list0 e) = foldl f s (reverse l)
fun foldli [e ::: Type] [s ::: Type] (f : int -> e -> s -> s) s (l : list0 e) = (foldl (fn e (s,i) => (f i e s, i + 1)) (s,0) l).1
fun foldri [e ::: Type] [s ::: Type] (f : int -> e -> s -> s) s (l : list0 e) = (foldr (fn e (s,i) => (f i e s, i + 1)) (s,0) l).1

fun repeat [t ::: Type] n (e : t) =
    let
	fun repeat' n r =
	    if n > 0
	    then
		repeat' (n - 1) (e :: r)
	    else
		r
    in
	repeat' n []
    end

fun repeati [t ::: Type] n (f : int -> t) =
    let
	fun repeat' n r =
	    if n > 0
	    then
		let
		    val n' = n - 1
		in
		    repeat' n' (f n' :: r)
		end
	    else
		r
    in
	repeat' n []
    end


(*
fun foldlmap f l s =
    let
	fun process ls =
	    case ls of
		x :: xs => f x s :: process
		*)

end
