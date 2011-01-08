(* datatype bool = Basis.bool *)
type bool = Basis.bool
con option = Basis.option
type int = Basis.int
type string = Basis.string
type count0 = int
type count1 = int

fun error [a ::: Type] (msg : string) : a = Basis.error <xml>{[msg]}</xml>
val fixme [a ::: Type] : a = error "unimplemented"
