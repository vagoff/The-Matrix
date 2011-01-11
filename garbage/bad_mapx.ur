fun mapX [e] (f : e -> xbody) (xs : list0 e) : xbody =
	let
		fun proc xs =
			case xs of
				None => <xml/>
			  | x :: xs' => <xml>{f x}{proc xs'}</xml>
	in
		proc xs
	end
