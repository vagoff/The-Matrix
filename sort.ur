(* mergesort *)

open Compare

fun sort [e ::: Type] (cmp : compare) (ls : list e) : list e =
    let
	fun breakIntoPairs xs =
	    case xs of
	        [] => []
	      | x :: [] => x :: []
	      | x1 :: x2 ::xs =>
		    let
			val pair =
			    case cmp x1 x2 of
			        GT => x2 :: x1 :: []
			      | _ => x1 :: x2 :: []
		    in
			pair :: breakIntoPairs xs
		    end

	fun mergePairsLoop ls =
	    case ls of
		_ :: _ :: _ => mergePairsLoop (mergeListPairs xxs)
	      | _ => ls
	
	fun mergeListPairs ls =
	    case ls of
		xs :: ys :: rest => mergeTwoLists xs ys :: mergeListPairs rest
	      | _ => ls
	
	fun mergeTwoLists xs ys =
	    case (xs,ys) of
		([],_) => xs
	      | (_,[]) => ys
	      | (x :: xs, y :: ys) =>
		    case cmp x y of
			GT => y :: mergeTwoLists (x :: xs) ys
		      | _ => x :: mergeTwoLists xs (y :: ys)
    in
	mergePairsLoop (breakIntoPairs ls)
    end
