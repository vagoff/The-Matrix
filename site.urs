type format
type view_id
val main : unit -> transaction page (* matrix view *)
val views : view_id -> transaction page (* view saved view *)
val exports : format -> transaction page (* export matrix *)
val search : string -> transaction page (* fulltext search *)
