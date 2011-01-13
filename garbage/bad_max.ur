fun max [e] (cmp : compare e) (m : e) (ls : list0 e) : e =
    let
    	fun max' m xs =
	        case xs of
	        	[] => m
    	      | x :: xs' =>
                    case compare m x of
    		        	LT => max' x xs'
    		          | _ => max' m xs'
    in
    	max' m ls
    end

