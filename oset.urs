    open Compare (* [!] use functor? *) (* [!] use functor instead of class? *)
    con oset : Type -> Type
    val remove : e ::: Type -> compare e -> e -> oset e -> oset e
    val insertAfter : e ::: Type -> compare e -> e -> e -> oset e -> oset e
    val insertBefore : e ::: Type -> compare e -> e -> e -> oset e -> oset e
    val append : e ::: Type -> compare e -> e -> oset e -> oset e
    val prepend : e ::: Type -> compare e -> e -> oset e -> oset e
    val sort : e ::: Type -> compare e -> oset e -> oset e
    val sortBy : e ::: Type -> (e -> e -> order) -> oset e -> oset e
