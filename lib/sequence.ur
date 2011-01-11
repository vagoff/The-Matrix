signature SEQUENCE = sig

con t :: Type -> Type

val length : e ::: Type -> t e -> Base.count0
val concat : e ::: Type -> t e -> t e -> t e
val rev : e ::: Type -> t e -> t e -> t e
val reverse : e ::: Type -> t e
val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b
val flatten : e ::: Type -> t (t e) -> t e
val max : e ::: Type -> Compare.compare e -> e -> t e -> e
val min : e ::: Type -> Compare.compare e -> e -> t e -> e
val foldl : e ::: Type -> s ::: Type -> (e -> s -> s) -> s -> t e
val foldr : e ::: Type -> s ::: Type -> (e -> s -> s) -> s -> t e
val foldli : e ::: Type -> s ::: Type -> (int -> e -> s -> s) -> s -> t e
val foldri : e ::: Type -> s ::: Type -> (int -> e -> s -> s) -> s -> t e
val repeat : e ::: Type -> Base.count0 -> e -> t e
val repeati : e ::: Type -> Base.count0 -> (Base.count0 -> e) -> t e
val mapX : e ::: Type -> (e -> xbody) -> t e -> xbody
val foldMapX : e ::: Type -> s ::: Type -> (e -> xbody) -> s -> t e -> xbody * s
val depMapX : e ::: Type -> s ::: Type -> (e -> xbody) -> s -> t e -> xbody

end

structure SequenceTest = struct
	structure L0 : SEQUENCE = List0
	structure L1 : SEQUENCE = List1
	(* [!] structure ST : SEQUENCE = Stream *)
end
