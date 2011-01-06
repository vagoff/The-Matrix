structure Base = struct
    (* datatype bool = Basis.bool *)
    type bool = Basis.bool
    con option = Basis.option
    type int = Basis.int
    type string = Basis.string

    fun error [a ::: Type] (msg : string) : a = Basis.error <xml>{[msg]}</xml>
    val fixme [a ::: Type] : a = error "unimplemented"

end
