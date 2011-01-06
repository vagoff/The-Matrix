(*
fun e 10 n = n
  | e _ _ = 0
*)

fun c n = case n of
    0 => 1 (* e 10 20 *)
  | _ => d (n - 1)

and d m = case m of
    0 => 2
  | _ => c (m - 1)


val main n = return <xml><body>{cdata (show (c n))}</body></xml>
