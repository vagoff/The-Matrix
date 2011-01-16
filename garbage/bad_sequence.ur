fun sequence [e] [m ::: Type -> Type] (m : monad m) (xs : stream (m e)) -> m (stream e) =
    case xs of
        Nil => return Nil
      | Cons (mx,t) => x <- mx; return Cons (x, fn () => joinM (t ()))
