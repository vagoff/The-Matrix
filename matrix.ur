signature MATRIX = sig
    con m :: Type -> Type
    type index = int (* [!] embed in m? *)
    type folder = e ::: Type -> state ::: Type -> ((index * index) -> e -> state -> state) -> state -> m e -> state
    val buildFromList : e ::: Type -> list ((index * index) * e) -> m e
    val new : e ::: Type -> (unit -> e) -> index * index -> m e
    val foldForward : folder
    val foldBackward : folder
    val size : e ::: Type -> m e -> (int * int)
end

structure Matrix : MATRIX = struct
    open List0
    con m = fn t => { Cols: int, Rows: int, Data: list0 (list0 t) }
    type index = int (* [!] embed in m? *)
    type folder = e ::: Type -> state ::: Type -> ((index * index) -> e -> state -> state) -> state -> m e -> state

    fun new [e] filler (cols,rows) = { Cols = cols, Rows = rows, Data = repeat rows (repeat cols (filler ())) }
    fun foldForward [e] [state] f s m = foldli (fn j row s => foldli (fn i e s => f (i,j) e s) s row) s m.Data
    fun foldBackward [e] [state] f s m = foldri (fn j row s => foldri (fn i e s => f (i,j) e s) s row) s m.Data

    fun buildFromList [e] l =
	let
	    val sorted = sortBy (fn ((i,j),_) ((n,m),_) =>
		case cmp i n of
		    EQ => cmp j m
		  | LT => LT
		  | GT => GT) l

	    fun build ls rn row res =
	        case ls of
		    [] => reverse res
	          | ((i,j),a) :: rest =>
		        if i = rn then
			    build rest rn (a :: row) res
		        else
			    build rest (rn + 1) [] (reverse row :: res)
	in
	    build sorted 0 [] []
	end

    fun size [e] {Cols=c,Rows=r,...} = (c,r)


end

structure MatrixTest = struct
    val create : int = Matrix.foldForward (fn (i,j) e s => s + e) 0 (Matrix.new (2,2))
end
