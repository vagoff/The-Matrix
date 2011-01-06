datatype export = YAML | XML | CVS | SXML

val main : unit -> transaction page
val view : view_id -> transaction page
val export : export -> transaction page
val search : string -> transaction page
