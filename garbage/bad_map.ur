fun mp [a] [b] (f : a -> b) (xs : list0 a) : list0 b =
    case xs of
	[] => []
      | x :: xs' => f x :: mp f xs'
