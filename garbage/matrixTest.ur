    val create : int = Matrix.foldForward (fn (i,j) e s => s + e) 0 (Matrix.new (fn () => 1) (2,2))
