structure D = Dict.ClientDict

type matrix = dict cell

fun toString : (lang_id, feature_id) -> string = (toString lid ^ ":" ^ toString fid)

fun empty () = D.empty
fun lookup m pos = D.lookup m (toString pos)
fun update (lid,fid) cell m = D.insert (toString pos) cell m
