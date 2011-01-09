type matrix
val empty : unit -> matrix
val lookup : matrix -> (lang_id * feature_id) -> cell
val update : (lang_id * feature_id) -> cell -> matrix -> matrix
