fun g n = n + 0

fun f n = case n of
    | 0 => 1
    | 1 => 0
    | _ => n

val main n = return <xml><body>{cdata (show (f n))}</body></xml>
