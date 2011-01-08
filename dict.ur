signature DICT = sig

    type key
    type dict t

    val empty : t ::: Type -> dict t
    val isEmpty : t ::: Type -> dict t -> bool
    val lookup : t ::: Type -> dict t -> key -> option t
    val insert : t ::: Type -> key -> t -> dict t -> dict t

end

(*
functor AssMap(sig type key end) = struct
end

structure StringDict : DICT = AssMap(struct type key = string end)
structure IntDict : DICT = AssMap(struct type key = int end)
*)

structure ClientDict : DICT = struct
    open JsLib
    type dict t = js_obj
    fun empty = jsNewObj ()
    fun isEmpty = jsIsEmpty
    fun insert [t] k v d = jsInsert k v d
    fun lookup [t] d k =
	let
	    val ret = jsLookup d k
	in
	    if jsIsNull ret then
		None
	    else
		Some ret
end
