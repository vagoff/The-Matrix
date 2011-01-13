fun concat [e] (xs : list0 e) (ys : list0 e) : list0 e =
    let
	fun cat xs =
	    case xs of
		[] => ys
	      | x :: xs => x :: cat xs
    in
	cat xs
    end
