(* ordered set *)

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
	
    fun sort [e] cmp a b = Sort.sort a b
    fun sortBy [e] fn a b = Sort.sortBy fn a b

