fun composest [s] [a] [z] f g s =
    let
        val (a,s') = f s
    in
	g a s'
    end

fun composests [s] [a] (fs : list (a -> s -> a * s)) (a : a) (s : s) : a * s =
    case fs of
	[] => (a,s)
      | f :: fs' =>
        let
    	    val (a',s') = f a s
    	in
    	    sts fs' a' s'
    	end

fun seq [s] (fs : list (s -> s)) (s : s) : s =
    case fs of
	[] => s
      | f :: fs' => seq fs' (f s)

fun seq' [s] (s : s) (fs : list (s -> s)) : s = seq fs s

fun rset [rest] [nm] [t] v [[nm] ~ rest] r = (r -- nm) ++ { nm = v } (* [!] inefficient *)
fun rupd [rest] [nm] [t] f [[nm] ~ rest] r = (r -- nm) ++ { nm = f r.nm } (* [!] inefficient *)
