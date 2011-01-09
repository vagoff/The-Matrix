type js_obj

val jsNewObj : unit -> js_obj
val jsIsEmpty : js_obj -> bool
val jsInsert : t ::: Type -> string -> t -> js_obj -> js_obj
val jsLookup : t ::: Type -> js_obj -> string -> t
val jsRemove : t ::: Type -> string -> js_obj -> js_obj
