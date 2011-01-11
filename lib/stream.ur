(* Lazy emptieble lists *)

datatype stream e = Cons of (e, (unit -> stream e)) | Nil

fun concat [e] (xs : stream e) (ys : stream e) : stream e =
	case xs of
		Nil => Nil
	  | Cons of (e, t) => Cons (e, fn () => concat (t ()) ys)

fun mp [e] [b] (f : e -> b) (xs : stream e) : stream b =
	case xs of
		Nil => Nil
	  | Cons of (e, t) => Cons (f e, fn () => mp f (t ()))

fun foldl [e] [s] (f : e -> s -> s) (s : s) (xs : stream e) : stream e =
	case xs of
		Nil => st
	  | Cons of (x, t) =>
	  		let
	  			(x',st') = f x st
	  		in
	  			foldl f st' (t ())
	  		end

fun mapX

fun foldX

fun foldMap

fun depMap
