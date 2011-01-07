structure List0 = struct

con list0 = list

fun concat [e ::: Type] (xs : list0 e) (ys : list0 e) : list0 e =
    let
	fun cat xs =
	    case xs of
		[] => ys
	      | x :: xs => x :: cat xs
    in
	cat x
    end

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

fun foldlMapRev [a ::: Type] [b ::: Type] [s ::: Type] (f : a -> s -> (b,s)) (s : s) (xs : list a) (ys : list b) : (list b,s) =
    let
	fun proc xs ys s =
	    case xs of
		[] => (rev ys, s)
	      | x' :: xs' =>
		    let
			val (y,s') = f x' s
		    in
			proc xs' (y :: ys) s'
		    end
    in
	proc xs ys s
    end

end
