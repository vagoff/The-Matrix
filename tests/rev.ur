fun rev0 [e ::: Type] (xs : list e) (ys : list e) =
    case xs of
	x :: xs => []
      | [] => ys

fun rev1 [e ::: Type] (xs : list e) (ys : list e) =
    let
	fun rev1' xs ys =
	    case xs of
		[] => ys
	      | x :: xs => rev1' xs (x :: ys)
    in
	rev1' xs ys
    end


fun rev3 [e ::: Type] (xs : list e) (ys : list e) =
    case xs of
	[] => ys
      | x :: xs => @rev3 [e] xs (x :: ys)

fun rev4 [e ::: Type] (xs : list e) (ys : list e) =
    case xs of
	[] => ys
      | x :: xs => @@rev4 [e] xs (x :: ys)
