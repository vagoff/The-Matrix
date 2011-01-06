structure Base = struct
    (* datatype bool = Basis.bool *)
    type bool = Basis.bool
    con option = Basis.option
    type int = Basis.int
    type string = Basis.string

    fun err msg = error <xml>{[msg]}</xml>
    val fixme : t ::: Type -> t = err "unimplemented"

end
