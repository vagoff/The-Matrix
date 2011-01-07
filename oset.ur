(* ordered set *)

signature OSET = sig
    open Compare (* [!] use functor? *) (* [!] use functor instead of class? *)
    con oset : Type -> Type
    val remove : e ::: Type -> compare e -> e -> oset e -> oset e
    val insertAfter : e ::: Type -> compare e -> e -> e -> oset e -> oset e
    val insertBefore : e ::: Type -> compare e -> e -> e -> oset e -> oset e
    val append : e ::: Type -> compare e -> e -> oset e -> oset e
    val prepend : e ::: Type -> compare e -> e -> oset e -> oset e
end

structure Oset : OSET = struct
    
    con oset = List0.list0

    fun remove [e] cmp e s =
	let
	    fun rem xs =
		case xs of
		    [] => []
		  | x :: xs =>
			case cmp e x of
			    EQ => rem xs
			  | _ => x :: rem xs
	in
	    rem s
	end

    fun insertAfter [e] cmp item e s =
	let
	    fun ins xs =
		case xs of
		    [] => e :: []
		  | x :: xs =>
			case cmp item x of
			    EQ => x :: e :: xs
			  | _ => x :: ins xs
	in
	    ins s
	end

    fun insertBefore [e] cmp item e s =
	let
	    fun ins xs =
		case xs of
		    [] => e :: []
		  | x :: xs =>
			case cmp item x of
			    EQ => e :: x :: xs
			  | _ => x :: ins xs
	in
	    ins s
	end

    fun prepend [e] cmp e s = e :: s
    fun append [e] cmp e s = List0.concat s (e :: [])
	
end
