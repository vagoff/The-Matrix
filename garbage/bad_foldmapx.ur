fun foldMapX [e] [s] (f : e -> xbody) (st : s) (xs : list0 e) : xbody =
	let
		fun loop xs st =
			case xs of
				None => <xml/>
			  | x :: xs' =>
			  	let
			  		val (x',st') = f st
			  	in
			  		 <xml>{x'}{loop xs' st'}</xml>
			  	end
	in
		loop xs st
	end
