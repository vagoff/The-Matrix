(* todo: track various implementations separately from languages *)
(* todo: what about awk/nix/shell/...? *)

(*
    , Authors : option
    , LicenseName : option desc
    , LicenseText : option desc
    , LastRealeaseDate : time_stamp
    , CPAN url
    , planet RSS feed url (with type)
*)

functor Initialize(Tools : sig
		type st
		type url
		val add_enum_type : string -> list string -> st -> st
		val add_feature : { Caption : string, Title : string, Typ : string, Url : option url, Groups : list string } -> st -> st
		val add_lang : { Caption : string, Title : string, Url : option url } -> st -> st
		val add_interest : { User : string, Lang : string } -> st -> st
		val url : string -> url
	end) : sig
		val initialize : st -> st
	end = struct

		open Tools (* no open in lets :(((( *)
		open Fp
val initialize =
	let
		val add_type = add_enum_type
		fun add_features_with_groups t groups ls st = fold (fn (c,t) st => add_feature { Caption = c, Title = t, Typ = t, Groups = groups } st) st ls (* [!] fold f ls st? *)
		fun add_feature_group t name = add_features_with_groups t [name]
		fun add_features t = add_features_with_groups t []
		fun add_interesting_langs user ls st = fold (fn l st => add_interest { User = user, Lang = l } st) st ls
		fun add_more_langs ls st = fold (fn (c,t) => add_lang { Caption = c, Title = t, Url = nourl } st) st ls
		val nourl = None
	in	
    seq (
	
	    add_type "quality" ( "poor":: "moderate":: "best" ::[])::

	    add_type "bool" ( "yes":: "no" ::[])::
	
	    add_type "status"
	    	( ("academic/theoretical/poc", nourl) (* poc = proof-of-concept *)
	    	:: ("commercial", nourl)
	    	:: ("not released", nourl)
	    	:: ("not widely used", url "http://langpop.com")
	    	:: ("still in development", nourl)
	    	:: ("not released yet", nourl)
	    	::[])::
	    
	    add_feature_group "backend"
			(  ("neko", "can compile to Neko VM", nourl)
			:: ("llvm", "can compile to LLVM", nourl)
			:: ("dotnet", "can compile to .NET VM", nourl)
			:: ("jvm", "can compile to JVM", nourl)
			::[])::
		
	    add_feature_group "paradigm support"
			(  ("oop", "object oriented", nourl)
			:: ("pp", "procedural", nourl)
			:: ("fp", "functional", nourl)
			:: ("lp", "logic", nourl)
			:: ("relp", "relational", nourl)
			:: ("rp", "reactive", nourl)
			:: ("cs", "constraint solving", nourl)
			:: ("aop", "aspect oriented programming", nourl)
			:: ("ip", "intentional programming", nourl)
			:: ("lng", "language oriented programming", nourl)
			:: ("tab", "table oriented programming", nourl)
			::[])::
		
	    add_interesting_langs "vag"
			(  "prop", "kaya", "clay", "haxe", "bitc", "qi", "scheme"
			:: "python", "smalltalk", "strongtalk", "eiffel", "java", "javascript"
			:: "haskell", "clean", "disciple", "hume", "curry", "cilk", "boo", "cobra"
			:: "gbeta", "guru", "ometa", "agda", "cayenne", "cpl", "charity", "epigram"
			:: "flapjax", "webdsl", "mobl", "factor", "falcon", "lunascript", "coffeescript"
			:: "curl", "rebol", "xl", "gel"
			:: "lustre", "esterel"
			:: "groovy", "scala", "falcon"
			:: "sml", "vault", "ocaml"
			::[])::

	    add_more_langs ( "ada":: "d":: "c":: "c++":: "c#":: "ruby" ::[])::

	    add_feature_group "bool" "syntax"
			(  ("c-like", "c-like syntax")
			:: ("offside", "layout rule")
			:: ("semi", "frequent semicolons after statements")
			:: ("c-braces", "c-like braces")
			:: ("expr", "no statements:: only exressions")
			:: ("longstr", "supports long strings and docstrings")
			::[])::
		
		add_feature_group "bool", "political"
			(  ("nolck", "language is not in vendor lock-in")
			:: ("gpl", "GPL-like license")
			:: ("mit", "MIT-like license")
			::[])::

		add_feature_group "bool" "tools"
			(  ("refactor", "has at least one refactoring tool")
			:: ("docgen", "has at least one doc gen tool")
			:: ("ide", "has ready to use:: modern IDE")
			:: ("emacs", "has Emacs mode")
			:: ("vim", "has Vim plugin")
			:: ("eclipse", "has Eclipse plugin")
			::[])::

    	add_features "bool"
	        (  ("ty-fun", "type-level functions")
	        :: ("ty-rec", "type-level records")
	        :: ("hot", "higher-order types")
	        :: ("sot", "second-order types")
			:: ("t-unions", "type unions")
			:: ("t-sub", "subtyping")
			:: ("t-poly", "polymorphism")
			:: ("t-overload", "ad hoc polymorphism")
			:: ("single-disp", "single dispatch")
			:: ("multi-disp", "multiple dispatch")
			:: ("tce", "tail call elimination")
			:: ("tco", "tail call optimization")
 			:: ("proc", "free procedures")
 			:: ("mixins", "mixins or traits")
 			:: ("interfaces", "interfaces/signatures")
 			:: ("unbox", "auto unboxing or ""everything object""")
 			:: ("macros", "has macrosses")
 			:: ("side", "free side effects")
 			:: ("mut", "native mutable variables")
 			:: ("ref", "references")
 			:: ("ptr", "pointers")
 			:: ("hof", "higher order functions")
 			:: ("closure", "support closures")
 			:: ("gc", "garbage collector")
 			:: ("eff", "effect tracking by types")
 			:: ("pure", "pure: no side effects allowed")
 			:: ("mem", "memory management mentioned in types")
 			:: ("amm", "non-garbage collecting automatic memory management")
 			:: ("lispy", "lisp-like syntax")
 			:: ("statie", "static typing")
 			:: ("dynie", "dynamic typing or no typing")
 			:: ("edsl", "allows easy to encode EDSLs")
			:: ("have official specification")
			:: ("has at least one imeplementation that is not vendor lock-in")
			:: ("error reporting is satisfactory")
			:: ("infer", "type inference")
			:: ("princip", "principal types")
			:: ("parmod", "parametrized modules (functors)")
			:: ("submod", "nested modules")
			:: ("nestfn", "nested procedures")
			:: ("variant", "variant records (a.k.a. algebraics)")
			:: ("abstyp", "abstract types")
			:: ("structural", "structural record equivalence")
			:: ("defarg", "default arguments")
			:: ("optarg", "optional arguments")
			:: ("vararg", "variable arguments")
			:: ("deleg", "delegates")
			:: ("contra", "covariant/contravariant subtyping")
			:: ("ivar", "has I-vars:: as in oz/prolog")
			:: ("futures", "has futures")
			:: ("premise", "has builtin premises")
			:: ("lazy", "has builtin mechanizm for creating lazy suspensions")
			:: ("resi", "has builtin support for residuation")
			:: ("narr", "has builtin support for narrowing")
			:: ("backtracking", "has backtracking")
			:: ("map literals")
			:: ("option", "builtin nullable types")
			:: ("nulls", "all types contains null")
			:: ("sync", "builtin syncronization")
			:: ("immut", "support for immutable data structures")
			:: ("compr", "list/array comprehensions")
			:: ("serial", "builtin serialization")
			:: ("str-interp", "string interpolation")
			:: ("rec-args", "record-as-arguments conversion support")
			:: ("goto", "fully featured labels and GOTO operator")
			:: ("delcc", "delimited continuations")
			:: ("partcc", "partial continuations")
			:: ("cc", "full multishot first-class continuations")
			:: ("excp", "exceptions")
			:: ("final", "finally in try/catch")
			:: ("excoro", "correct exception support in coroutining constructions")
			:: ("gen", "generators")
			:: ("iter", "iterators")
			:: ("compr", "sequence comprehensions")
			:: ("ndsl", "full EDSL support")
			:: ("pdsl", "partial EDSL support")
			:: ("macro", "macrosses")
			:: ("stag", "staging compilation")
			:: ("code", "two stage code transformation")
			:: ("scoro", "symmetric coroutines")
			:: ("thread", "builtin support for threading")
			:: ("acoro", "asymmetric coroutines")
			:: ("looplab", "labels in break and coninue statement")
			:: ("destra", "destructuring assignment (pattern at left hand side)")
			:: ("multia", "multiple assignment (a = b = c)")
			:: ("concat", "concatenative")
			:: ("homo", "homoiconicity")
			:: ("stat", "static typing")
			:: ("dyn", "dynamic typing")
			:: ("strtyp", "strong typing")
			:: ("wtyp", "weak typing")
			:: ("virt", "virtual methods/late binding support")
			:: ("notun", "has 'message not understood'")
			:: ("gen", "generic programming (parametric polymorphism)")
			:: ("bgen", "bounded polymorphism")
			:: ("poly", "polytypic programming")
			:: ("over", "overloading (ad-hoc polymorphism)")
			:: ("mult", "multiple dispatch")
			:: ("coer", "implicit type coersions")
			:: ("cast", "explicit type casts")
			:: ("lscop", "lexical scope")
			:: ("dscop", "dynamic scope")
			:: ("ldscop", "both static and dynamic scope")
			::[])::
		
	[])
	end

end
