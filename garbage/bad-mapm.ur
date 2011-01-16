fun mapM [e] [m ::: Type -> Type] (m : monad m) (f : e -> m r) (xs : stream e) : m (stream r) =
    case xs of
        Nil => return xs
      | Cons (x,t) =>
            x' <- f x;
            return Cons (x', fn () => 
