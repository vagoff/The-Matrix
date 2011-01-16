(* Lazy emptieble lists *)

datatype stream e = Cons of (e, (unit -> stream e)) | Nil

(* local *)

fun convert [e] (xs : stream e) : list0 e =
    case xs of
        Nil => []
        Cons (x,t) => x :: convert (t ())

fun concat [e] (xs : stream e) (ys : stream e) : stream e =
	case xs of
		Nil => ys
	  | Cons of (x, t) => Cons (x, fn () => concat (t ()) ys)

fun rev [e] (xs : stream e) (ys : stream e) : stream e =
    case xs of
	    Nil => ys
      | Cons (x,t) => rev (t ()) (Cons (x, fn () => ys))

fun reverse [e] (l : stream e) = rev l Nil

fun mp [e] [b] (f : e -> b) (xs : stream e) : stream b =
	case xs of
		Nil => Nil
	  | Cons of (x, t) => Cons (f x, fn () => mp f (t ()))

fun flatten [e] (xxs : stream e) =
    case xxs of
        Nil => Nil
      | Cons (xs,t) => concat xs (flatten (t ()))

fun repeat [e] (n : Base.count0) (e : e) =
    if n > 0 then
        Cons (x, fn () => repeat (n - 1) x)
    else
        Nil

fun repeati [e] (n : Base.count0) (g : Base.count0 -> e) : stream e =
    let
        repeati' i =
            if i > n then
                Nil
            else
                Cons (f i, fn () => repeati' (i + 1))

fun mapX [e] (f : e -> xbody) (xs : stream e) : xbody =
    case xs of
        Nil => <xml/>
        Cons (x,t) => <xml>{f x}{mapX f (t ())}</xml>

fun length [e] (xs : stream e) = L0.length (convert xs)
fun foldl [e] [s] (f : e -> s -> s) (s : s) (xs : stream e) = Lo.foldl f s (convert xs)
fun foldr [e] [s] (f : e -> s -> s) (s : s) (xs : stream e) = L0.foldr f s (convert xs)
fun foldli [e] [s] (f : e -> s -> s) (s : s) (xs : stream e) = L0.foldli f s (convert xs)
fun foldri [e] [s] (f : e -> s -> s) (s : s) (xs : stream e) = L0.foldri f s (convert xs)

fun foldMapX [e] [s] (f : e -> xbody) (s : s) (xs : stream e) : xbody * s =
	let
		fun foldMapX' xs res st =
			case xs of
				Nil => (res,st)
			  | Cons (x,t) =>
			  	let
			  		val (x',st') = f (t ())
			  	in
			  		 foldMapX' xs' <xml>{res}{x'}</xml> st'
			  	end
	in
		foldMapX' xs <xml/> s
	end

fun depMapX [e] [s] (f : e -> xbody) (s : s) (xs : stream e) : xbody = fst (foldMapX f s xs)

fun mapM [e] [m ::: Type -> Type] (m : monad m) (f : e -> m r) (xs : stream e) : m (stream r) =
    let
        fun mapM' xs res =
    		case xs of
	    		Nil => res
		      | Cons (x,t) => mapM' (t ()) (ys <- res ; y <- f e ; return (Cons (y,fn () => ys)))
    in
        res <- mapM' xs (return Nil);
        return (reverse res)
    end

fun mapMX [e] [m ::: Type -> Type] (m : monad m) (f : e -> m xbody) (xs : stream e) : m xbody =
    let
        fun mapMX' xs res =
    		case xs of
	    		Nil => res
		      | Cons (x,t) => mapM' (t ()) (ys <- res ; y <- f e ; return <xml>{ys}{y}</xml>)
    in
        mapMX' xs (return <xml/>)
    end
