signature TUPLE = sig

val fst : a ::: Type -> b ::: Type -> a * b -> a
val snd : a ::: Type -> b ::: Type -> a * b -> b

end

structure Tuple : TUPLE = struct

fun fst [a] [b] (a,b) = a
fun snd [a] [b] (a,b) = b

end
