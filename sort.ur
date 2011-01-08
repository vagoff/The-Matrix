(* mergesort *)

open Compare

fun sortBy [e] (cmpFn : e -> e -> order) (ls : list e) : list e =
    let
	fun breakIntoPairs xs =
	    case xs of
	        [] => []
	      | x :: [] => (x :: []) :: []
	      | x1 :: x2 ::xs =>
		    let
			val pair =
			    case cmpFn x1 x2 of
			        GT => x2 :: x1 :: []
			      | _ => x1 :: x2 :: []
		    in
			pair :: breakIntoPairs xs
		    end

	and mergePairsLoop ls =
	    case ls of
	        [] => []
	      | xs :: [] => xs
	      | _ :: _ :: _ => mergePairsLoop (mergeListPairs ls)
	
	and mergeListPairs ls =
	    case ls of
		xs :: ys :: rest => mergeTwoLists xs ys :: mergeListPairs rest
	      | _ => ls
	
	and mergeTwoLists xs ys =
	    case (xs,ys) of
		([],_) => xs
	      | (_,[]) => ys
	      | (x :: xs, y :: ys) =>
		    case cmpFn x y of
			GT => y :: mergeTwoLists (x :: xs) ys
		      | _ => x :: mergeTwoLists xs (y :: ys)
    in
	mergePairsLoop (breakIntoPairs ls)
    end

fun sort [e] (cmp : compare e) (ls : list e) : list e = sortBy compare ls
