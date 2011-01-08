fun chopRev [e ::: Type] (xs : list0 e) (ys : list0 e) : option (list0 e * e) =
    let
	fun chopRev' xs ys =
	    case xs of
		[] => None
	      | x :: [] => Some (ys, x)
	      | x :: xs' => chopRev' xs' (x :: ys)
    in
	chopRev' xs ys
    end
