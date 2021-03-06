    open List0
    open Base
    open Sort

    con m t = { Cols: int, Rows: int, Data: list0 (list0 t) }
    type index = int (* [!] embed in m? *)
    type folder = e ::: Type -> state ::: Type -> ((index * index) -> e -> state -> state) -> state -> m e -> state

    fun new [e] filler (cols,rows) = { Cols = cols, Rows = rows, Data = repeat rows (repeat cols (filler ())) }
    fun foldForward [e] [state] f s m = foldli (fn j row s => foldli (fn i e s => f (i,j) e s) s row) s m.Data
    fun foldBackward [e] [state] f s m = foldri (fn j row s => foldri (fn i e s => f (i,j) e s) s row) s m.Data

    fun buildFromList [e] ls =
	let
	    val sorted = sortBy (fn ((i,j),_) ((n,m),_) =>
		case compare i n of
		    EQ => compare j m
		  | LT => LT
		  | GT => GT) ls

	    fun build ls rn row res =
	        case ls of
		    [] => reverse res
	          | ((i,j),a) :: rest =>
		        if i = rn then
			    build rest rn (a :: row) res
		        else
			    build rest (rn + 1) [] (reverse row :: res)
	    val result = build sorted 0 [] []
	in
	    { Rows = length result
	    , Cols = max 0 (mp length result)
	    , Data = result
	    }
	end

    fun size [e] {Cols=c,Rows=r,...} = (c,r)

