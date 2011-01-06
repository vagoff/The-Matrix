fun f n = (fn n : int : int : int : int => n + 1) n

val main n = return <xml><body>{cdata (show (f n))}</body></xml>
