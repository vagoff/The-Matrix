    let
	fun max' m xs =
	    case xs of
		[] => m
	      | x :: xs' =>
	    	    case compare m x of
			LT => max' x xs'
		      |_ => max' m xs'
    in
	max' x xs
    end
