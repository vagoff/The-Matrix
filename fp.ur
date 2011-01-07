signature FP = sig
    fun st : s ::: Type -> z ::: Type -> (s -> (a,s)) -> (a -> s -> z) -> (s -> z)
    fun sts : s ::: Type -> list (a -> s -> (a,s)) -> (a -> s -> (a,s))
    fun seq : s ::: Type -> list (s -> s) -> (s -> s)
end
structure Fp : FP = struct

fun st [s] [z] f g =
    let
        val (a,s') = f s
    in
	g s'
    end

fun sts [s] fs a s =
    case fs of
	[] => (a,s)
      | f :: fs' =>
        let
    	    val (a',s') = f a s
    	in
    	    seq fs' a' s'
    	end

fun seq [s] fs s =
    case fs of
	[] => s
      | f :: fs' => seq fs' (f s)

end

