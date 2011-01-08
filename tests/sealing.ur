signature S1 = sig
        type t
        val x : t
        val y : t
end

signature S2 = sig
        val x : int
        val y : int
end

signature S3 = sig
        val x : int
end

structure M0 = struct
	type t = int
        val x = 0
        val y = 0
        val z = 0
end

structure M1 : S1 = M0
structure M2 : S2 = M0
structure M3 : S3 = M0
structure M4 : S3 = M2

val v = 1
val w = 2
