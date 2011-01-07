(* todo: track various implementations separately from languages *)

init
    val langs =
    val features =

    ts = 

    add_type "quality" [ "poor", "moderate", "best" ]
    add_type "bool" [ "yes", "no" ]

    add_feature_group "Type System"
    
        [ ("ty-fun", "type-level functions")
        , ("ty-rec", "type-level records")
        , ("hot", "higher-order types")
        , ("sot", "second-order types")
        ]

    add_type "status" [ "academic/poc", "commercial", "not released", "not widely used" ]
    
    add_type "backend" [
	, ("neko", "can compile to Neko VM", nourl)
	, ("llvm", "can compile to LLVM", nourl)
	, ("dotnet", "can compile to .NET VM", nourl)
	, ("jvm", "can compile to JVM", nourl)
	]

    add_type "paradigm"
	[ ("object oriented", nourl)
	, ("procedural", nourl)
	, ("functional", nourl)
	, ("logic", nourl)
	, ("relational", nourl)
	, ("reactive", nourl)
	, ("constraint solving", nourl)
	]


			, "academic/poc"
			, "still in development, not released yet"

    add_interesting_langs "vag"
	[ "prop", "kaya", "clay", "haxe", "bitc", "qi", "scheme",
	, "python", "smalltalk", "strongtalk", "eiffel", "java", "javascript",
	, "haskell", "clean", "disciple", "hume", "curry", "cilk", "boo", "cobra",
	, "gbeta", "guru", "ometa", "agda", "cayenne", "cpl", "charity", "epigram"
	, "flapjax", "webdsl", "mobl", "factor", "falcon", "lunascript", "coffeescript"
	, "curl", "rebol",
	, "lustre", "esterel"
	, "groovy", "scala", "falcon"
	, "sml", "vault"
	]

    add_more_langs [ "ada", "d", "c", "c++", "c#" ]

    syntax =
	[ ("c-like", "c-like syntax")
	, ("offside", "layout rule")
	, ("semi", "frequent semicolons after statements")
	, ("c-braces", "c-like braces")
	, ("expr", "no statements, only exressions")
	, ("longstr", "supports long strings and docstrings")
	]

    tools =
	[ ("refactor", "has at least one refactoring tool")
	, ("docgen", "has at least one doc gen tool")
	, ("ide", "has ready to use, modern IDE")
	, ("emacs", "has Emacs mode")
	, ("vim", "has Vim plugin")
	, ("eclipse", "has Eclipse plugin")
	]

    features =
			[ ("t-unions", "type unions",
			, ("t-sub", "subtyping"
 			, ("t-poly", "polymorphism"
			, ("t-overload", "ad hoc polymorphism"
 			, ("single-disp", "single dispatch"
 			, ("multi-disp", "multiple dispatch"
 			, ("oop", "oop support"
			, ("tce", "tail call elimination"
			, ("tco", "tail call optimization"
 			, ("proc", "free procedures"
 			, ("mixins", "mixins or traits"
 			, ("interfaces", "interfaces/signatures"
 			, ("unbox", "auto unboxing or \"everything object\""
 			, ("macros", "has macrosses")
 			, ("side", "free side effects")
 			, ("mut", "native mutable variables"
 			, ("hof", "higher order functions"
 			, ("closure", "support closures")
 			, ("gc", "garbage collector")
 			, ("lispy", "lisp-like syntax")
 			, ("statie", "static typing")
 			, ("dynie", "dynamic typing or no typing")
 			, ("edsl", "allows easy to encode EDSLs")
			, ("have official specification")
			, ("language is not in vendor lock-in")
			, ("has at least one imeplementation that is not vendor lock-in")
			, ("error reporting is satisfactory")
			, ("infer", "type inference")
			, ("structural", "structural record equivalence")
			, ("defarg", "default arguments")
			, ("optarg", "optional arguments")
			, ("vararg", "variable arguments")
			, ("deleg", "delegates")
			, ("contra", "covariant/contravariant type matching")
			, ("ivar", "has I-vars, as in oz/prolog")
			, ("futures", "has futures")
			, ("lazy", "has builtin mechanizm for creating lazy suspensions")
			, ("resi", "has builtin support for residuation")
			, ("narr", "has builtin support for narrowing")
			, ("premise", "has builtin premises")
			, ("backtracking", "has backtracking")
			, ("map literals")
			, ("option", "builtin nullable types")
			, ("nulls", "all types contains null")
			, ("sync", "builtin syncronization",)
			, ("immut", "support for immutable data structures")
			, ("compr", "list/array comprehensions")
			, ("serial", "builtin serialization")
			, ("str-interp", "string interpolation")
			, ("rec-args", "record-as-arguments conversion support")
			, ("full-labels", "full features labels")
			, ("loop-labels", "labels in break and coninue statement")
			, ("destr-ass", "destructuring assignment (pattern at left hand side)")
			, ("multi-ass", "multiple assignment (a = b = c)")
			, ("concat", "concatenative")
			, ("homo", "homoiconic")
			]
