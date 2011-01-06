
datatype cont 'i = C ('i -> 'o)

fun run a =
    let

	fun stage1 c n = case n of
	    0 => 1
	  | _ => case c of C f => f stage2 (n - 1)

	fun stage2 f n = case n of
	    0 => 2
	  | _ => f stage1 (n - 1)
    in
	stage1 stage2 a
    end

val main n = return <xml><body>{cdata (show (run (n + 10)))}</body></xml>
