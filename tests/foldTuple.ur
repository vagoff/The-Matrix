(* tuple folder *)

class isanum :: Name -> Type
val isanum 0 = ()
val isanum 1 = ()
val isanum 2 = ()

class isnil :: Type -> Type
val isnil {} = true
val isnil {nm} = false

class decons

decons : K ==> z ::: K -> t ::: {Unit} -> (nm -> z) -> ({} -> z) -> z


