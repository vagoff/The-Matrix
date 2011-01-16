fun length [e] xs =
    let
        length xs n =
            case xs of
                Nil => n
              | Cons (_,t) => length (t ()) (n + 1)
    in
        length xs 0
    end
