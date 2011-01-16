fun foldl [e] [s] (f : e -> s -> s) (s : s) (xs : stream e) : s =
	case xs of
		Nil => st
	  | Cons of (x, t) =>
	  		let
	  			(x',st') = f x st
	  		in
	  			foldl f st' (t ())
	  		end
