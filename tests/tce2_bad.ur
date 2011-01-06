fun top x =
    let
	fun c n = case n of
	    0 => d n + 10
	  | 1 => c n + 20
	  | 2 => 20
	  | _ => d (n - 1)

	and d m = case m of
	    0 => c m + 30
	  | 1 => d m + 40
	  | 2 => 30
	  | _ => c (m - 1)
    in
	c x
    end

val main n = return <xml><body>{cdata (show (top (n + 10)))}</body></xml>
