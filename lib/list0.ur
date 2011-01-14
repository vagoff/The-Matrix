con list0 = list

open Compare
con count0 = Base.count0

fun length [e] (xs : list0 e) : count0 =
    let
    	fun len xs l =
    	    case xs of
        		[] => l
	          | _ :: xs' => len xs' (l + 1)
    in
	    len xs 0
    end

fun concat [e] (xs : list0 e) (ys : list0 e) : list0 e =
    let
    	fun cat xs ys =
	        case xs of
	    	    [] => ys
    	      | x :: xs' => cat xs' (x :: ys)
    in
    	cat (reverse xs) ys
    end

fun flatten [e] (xs : list0 (list0 e)) : list0 e = concat
	let
		fun flatten' xs res =
			case xs of
				None => res
			  | x :: xs' => flatten' xs' (concat x res)
			
	in
		flatten' (reverse xs) []
	end

fun rev [e] (xs : list0 e) (ys : list0 e) : list0 e =
    case xs of
	    [] => ys
      | x :: xs => rev xs (x :: ys)

fun reverse [e] (l : list0 e) = rev l []

fun mp [a] [b] (f : a -> b) (xs : list0 a) : list0 b =
    let
        fun mp' xs ys =
            case xs of
    	        [] => ys
              | x :: xs' => mp' xs' (f x : ys)
    in
        reverse (mp' xs [])
    end

fun foldl [e] [s] (f : e -> s -> s) (s : s) (xs : list0 e) : s =
    case xs of
	    [] => s
      | x :: xs => foldl f (f x s) xs

fun foldr [e] [s] (f : e -> s -> s) (s : s) (l : list0 e) = foldl f s (reverse l)
fun foldli [e] [s] (f : int -> e -> s -> s) s (l : list0 e) = (foldl (fn e (s,i) => (f i e s, i + 1)) (s,0) l).1
fun foldri [e] [s] (f : int -> e -> s -> s) s (l : list0 e) = (foldr (fn e (s,i) => (f i e s, i + 1)) (s,0) l).1

fun max [e] (cmp : compare e) (m : e) (ls : list0 e) : e =
    fold (fn e m =>
        case compare m x of
            LT => x
          | _ => m) m ls

fun repeat [t] n (e : t) =
    let
    	fun repeat' n r =
    	    if n > 0 then
        		repeat' (n - 1) (e :: r)
	        else
    	    	r
    in
	    repeat' n []
    end

fun repeati [t] n (f : int -> t) =
    let
    	fun repeat' n r =
	        if n > 0 then
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

fun foldlMapRev [a] [b] [s] (f : a -> s -> b * s) (s : s) (xs : list a) (ys : list b) : list b * s =
    let
    	fun proc xs ys s =
	        case xs of
		        [] => (reverse ys, s)
    	      | x' :: xs' =>
	        	    let
			            val (y,s') = f x' s
        		    in
            			proc xs' (y :: ys) s'
        		    end
    in
    	proc xs ys s
    end

fun mapX [e] (f : e -> xbody) (xs : list0 e) : xbody =
	let
		fun mapX' xs res =
			case xs of
				None => res
			  | x :: xs' => mapX' xs' <xml>{res}{f x}</xml>
	in
		mapX' xs <xml/>
	end

fun foldMapX [e] [s] (f : e -> xbody) (s : s) (xs : list0 e) : xbody * s =
	let
		fun foldMapX' xs res st =
			case xs of
				None => (res,st)
			  | x :: xs' =>
			  	let
			  		val (x',st') = f st
			  	in
			  		 foldMapX' xs' <xml>{res}{x'}</xml> st'
			  	end
	in
		foldMapX' xs <xml/> s
	end

fun depMapX [e] [s] (f : e -> xbody) (s : s) (xs : list0 e) : xbody = fst (foldMapX f s xs)

fun mapM [e] [m] (m : monad m) (f : e -> m r) (xs : list0 e) : m (list r) = (* mapMRev? *)
    let
        fun mapM' xs res =
    		case xs of
	    		None => res
		      | x :: xs' => mapM' xs' (ys <- res ; y <- f e ; return (y :: ys))
    in
        res <- mapM' xs (return []);
        return (reverse res)
    end

fun mapMX [e] [m] (m : monad m) (f : e -> m xbody) (xs : list0 e) : m xbody =
    let
        fun mapMX' xs res =
    		case xs of
	    		None => res
		      | x :: xs' => mapM' xs' (ys <- res ; y <- f e ; return <xml>{ys}{y}</xml>)
    in
        mapMX' xs (return <xml/>)
    end
