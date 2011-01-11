signature EQUAL = sig
    class equal
    val equal_int : equal int
    val equal_char : equal char
    val equal_float : equal float
    val equal_bool : equal bool
end
structure Equal : Equal = struct
    con equal t = t -> t -> bool
    fun equal_int a b = a = b
    fun equal_char a b = a = b
    fun equal_float a b = a = b
    fun equal_bool a b = a = b
end
