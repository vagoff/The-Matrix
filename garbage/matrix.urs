    con m :: Type -> Type
    type index = int (* [!] embed in m? *)
    type folder = e ::: Type -> state ::: Type -> ((index * index) -> e -> state -> state) -> state -> m e -> state
    val buildFromList : e ::: Type -> list ((index * index) * e) -> m e
    val new : e ::: Type -> (unit -> e) -> index * index -> m e
    val foldForward : folder
    val foldBackward : folder
    val size : e ::: Type -> m e -> (int * int)
