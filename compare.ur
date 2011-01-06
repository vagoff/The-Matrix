(* total order *)

(* uses: ord, strsub, strlen *)

signature COMPARE = sig

    datatype order = LT | EQ | GT

    class compare

    val compare : t ::: Type -> compare t -> t -> t -> order

    val compare_int : compare int
    val compare_float : compare float
    val compare_string : compare string
    val compare_list : a ::: Type -> compare a -> compare (list a)
end

structure Compare : COMPARE = struct

    datatype order = LT | EQ | GT

    con compare t = t -> t -> order
    class compare = compare

    fun compare [t] cmp a b = cmp a b

    fun compare_int x y = if x < y then LT else if x > y then GT else EQ

    fun compare_float x y = if x < y then LT else if x > y then GT else EQ

    fun compare_char x y = compare_int (ord x) (ord y)

    fun compare_string x y =
	let
	    val xlen = strlen x
	    val ylen = strlen y
	    fun comp i j =
		if i >= xlen then
		    if j >= ylen then
			EQ
		    else
			LT
		else
		    if j >= ylen then
			GT
		    else
			case compare_char (strsub x i) (strsub y j) of
			    EQ => comp (i + 1) (j + 1)
			  | LT => LT
			  | GT => GT
	in
	    if xlen = 0 then
		if ylen = 0 then
		    EQ
		else
		    LT
	    else
		if ylen = 0 then
		    GT
		else
		    comp 0 0
	end

    fun compare_list [a] cmp xs ys =
	let
	    fun comp xs ys =
		case (xs,ys) of
		    ([], []) => EQ
		  | ([], _) => LT
		  | (_, []) => GT
		  | (x :: xs', y :: ys') =>
			case cmp x y of
			    EQ => comp xs' ys'
			  | LT => LT
			  | GT => GT
	in
	    comp xs ys
	end
end
