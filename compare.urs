    datatype order = LT | EQ | GT

    class compare

    val compare : t ::: Type -> compare t -> t -> t -> order

    val compare_int : compare int
    val compare_float : compare float
    val compare_string : compare string
    val compare_list : a ::: Type -> compare a -> compare (list a)
