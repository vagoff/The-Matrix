(* todo: track various implementations separately from languages *)
(* todo: what about awk/nix/shell/...? *)
(* todo: support for language aliases (due to renaming, etc) *)
(* todo: check labels for uniqueness automatically at configuring stage *)

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
			(  "Prop":: "Kaya":: "Clay":: "Haxe":: "Qi":: "Haskell":: "Clean":: "Omega"
			:: "Python":: "Smalltalk":: "Strongtalk":: "Eiffel":: "Java":: "JavaScript":: "EcmaScript"
			:: "Haskell":: "Clean":: "Disciple":: "Hume":: "Curry":: "Cilk":: "Boo":: "Cobra"
			:: "GBeta":: "Guru":: "OMeta":: "Agda":: "Agda2":: "Cayenne":: "CPL":: "Charity":: "Epigram"
			:: "Flapjax":: "WebDSL":: "Mobl":: "Lunascript":: "Coffeescript":: "Kodu"
			:: "Curl":: "Rebol":: "XL":: "GEL":: "Ur":: "Lustre":: "Esterel":: "ooc":: "orc"
			:: "Groovy":: "Scala":: "Zice":: "Falcon":: "SML":: "Vault":: "Ocaml":: "F#"
			::[])::

	    add_more_langs
	        (  "Ada":: "D":: "C":: "C++":: "C#":: "Ruby":: "Go":: "Io":: "Oz":: "Scheme"
            :: "Duby":: "Newspeak"::  "Objective-J":: "BitC":: "PyPy":: "Clojure"
            :: "Fancy":: "Coherence":: "Subtext":: "Noop":: "Factor":: "E":: "Caja":: "Slate"
            :: "AmbientTalk":: "Thyrd":: "Cola":: "Gosu":: "Stratified Javascript"
            :: "Jsws":: "Frink":: "Dalvik":: "Trylon":: "Ioke":: "Coc"
            :: "Occam":: "Pict":: "Prolog":: "Icon":: "Self":: "Forth"
            :: "Pascal":: "Oberon":: "Zonnon":: "Factor":: "Falcon"
            ::[])::

	    add_feature_group "bool" "syntax"
			(  ("c-like", "c-like syntax")
			:: ("offside", "layout rule")
			:: ("semi", "frequent semicolons after statements")
			:: ("c-braces", "c-like braces")
			:: ("expr", "no statements, only exressions")
			:: ("longstr", "supports long strings and docstrings")
			::[])::
		
		add_feature_group "bool" "political"
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
			:: ("spec", "have official specification")
			:: ("lck", "has at least one imeplementation that is not vendor lock-in")
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
			:: ("excoro", "correct exception support in coroutining")
			:: ("gen", "has builtin support for generators")
			:: ("iter", "has builtin support for iterators")
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
			:: ("concat", "concatenative")
			:: ("boot", "has compiler written in itself")
			:: ("eso", "esoteric")
			:: ("edu", "educational")
			::[])::
		
	[])
	end

end

(*
			:: ("short", "extra short definitions (lisp,smalltalk,haskell,forth)")
			:: ("error reporting is satisfactory")
			:: ("map literals") -- what is it? i've forgot entirely :(
*)

(*
Array Programming Languages

    * Fortran
    * Analytica
    * Chapel
    * APL
    * FISh
    * F
    * A+
    * F-Script
    * Fortress
    * IDL
    * J
    * K
    * PDL
    * R
    * NESL
    * MATLAB (Matrix Laboratory)
    * S-Lang
    * Octave
    * Nial
    * SAC
    * ZPL
    * X10

Aspect Oriented Programming Languages

    * AspectLua
    * AspectJ
    * CaesarJ
    * Object Teams
    * E
    * Aspect C++
    * Compose

Authoring Programming Languages

    * PILOT
    * Bigwig
    * TUTOR
    * Coursewriter

Assembly Languages

    * ASEM-51
    * AKI
    * ASCENT
    * ASPER
    * BAL
    * C--
    * COMPASS
    * Emu8086
    * EDTASM
    * FAP
    * FASM
    * GAS
    * HLA
    * HLASM
    * LC-3
    * Linoleum
    * MACRO-11
    * MACRO-20
    * MACRO-32
    * MASM
    * MI
    * MIPS
    * Motorola 68KAssembly of CPUs
    * NASM
    * NEAT
    * PAL-III
    * PASM
    * RosAsm
    * Sphinx
    * TASM
    * Yasm

Command Line Interface Programming Languages

    * 4DOS
    * .bat
    * Windows PowerShell
    * CHAIN
    * CLIST
    * DCL,/li>
    * DOS Batch Language
    * CMS EXEC
    * EXEC 2
    * JCL
    * sh
    * csh
    * Ch
    * tcsh
    * bash
    * ksh
    * zsh
    * Rc
    * Es shell
    * REXX
    * SCLI
    * SsCLI
    * TACL

Compiled Programming Languages

    * Ada
    * ALGOL
    * Ateji PX
    * BASIC
    * C
    * C++
    * C#
    * CLEO
    * CLIPPER 5.3
    * Clush
    * COBOL
    * CLisp
    * Cobra
    * Corn
    * Curl
    * D
    * DASL
    * Deplhi
    * DIBOL
    * Dylan
    * dylan.NET
    * Ecere C
    * Eiffel
    * Factor
    * Forth
    * Fortran
    * Go
    * Haskell
    * Harbour
    * Java
    * JOVIAL
    * LabVIEW
    * Nemerle
    * Objective-C
    * Pascal
    * Plus
    * ppC++
    * Python
    * RPG
    * Scheme
    * SmallTalk
    * ML
    * Turing
    * Urq
    * Visual Basic
    * Visual FoxPro
    * Visual Prolog
    * WinDev
    * X++
    * XL
    * Z++

Concurrent Programming Languages

    * Ada
    * ChucK
    * Cilk
    * C Omega
    * Clojure
    * ConcurrentLua
    * Concurrent Pascal
    * Corn
    * Curry
    * E
    * Eiffel
    * Erlang
    * Go
    * Java
    * Join-Calculus
    * Joule
    * Limbo
    * MultiLisp
    * occam
    * Oz
    * Pict
    * SALSA
    * Scala
    * SR

Dataflow Programming Languages

    * Hartman Pipelines
    * G
    * Lucid
    * Max
    * Prograph
    * Pure Data
    * Vee
    * VisSim
    * WebMethods Flow
    * Monk
    * Oz
    * VHDL

Data Oriented Programming Languages

    * Clarion
    * Clipper
    * dBase
    * MUMPS
    * SPARQL
    * SQL
    * Tutorial D
    * Visual FoxPro
    * WebQL

Educational Programming Languages

    * Scratch
    * Etoys
    * Squeak
    * BlueJ
    * Greenfoot
    * NetBeans
    * Scheme
    * Logo
    * Common Lisp
    * newLISP
    * Gambas
    * SiMPLE
    * Microsoft Small Basic
    * BASIC-256
    * Visual Basic .Net (one of the most popular computer programming langauges for beginners)
    * Alice (the most popular language which is used in computer programming for kids)
    * AgentSheets
    * Baltie
    * E-Slate
    * CiMPLE
    * Hackey Hack
    * Guido van Robot
    * Kodu
    * Karel
    * Mama
    * Pascal
    * Lego Mindstorms
    * RoboMind
    * Phrogram
    * Stagecast Creator
    * Curry
    * Haskel
    * A++
    * Oz
    * Qi II
    * M2001

Data Structured Programming Languages

    * dBase
    * SQL
    * Clarion
    * MUMPS
    * SPARQL
    * Fox Pro
    * Clipper
    * WebQL

Declarative Programming Languages

    * Ant
    * Lustre
    * Modelica
    * xBase
    * MetaPost
    * DASL
    * XSL Transformations
    * Prolog
    * Poses++

Extension Programming Languages

    * Ateji PX
    * AutoLISP
    * CAL
    * C/AL
    * DML
    * Guile
    * Lua
    * OptimJ
    * Python
    * REXX
    * Ruby
    * S-Lang
    * SQL
    * Tcl
    * Vimscript
    * VBA
    * Windows PowerShell

Esoteric Programming Languages

    * Whitespace
    * Chef
    * Knlingon
    * Befunge
    * Shakespeare
    * LOLCODE
    * FALSE
    * Piet
    * INTERCAL
    * Malbolge
    * SNUSP

Functional Programming Languages

    * Charity
    * Curl
    * Clean
    * F#
    * Haskell
    * Lisp
    * Hop
    * Mathematica
    * ML
    * Erlang
    * R
    * Spreadsheets
    * Kite
    * OPS5
    * Opal

Iterative Programming Languages

    * Python
    * Cobra
    * XL
    * Eiffel
    * Sather
    * Alphard
    * Icon
    * Aldor
    * Lua
    * C#
    * Lush
    * CLU

Logic Oriented Programming Languages

    * Leda
    * Janus
    * Poplog
    * Oz
    * Fril
    * CLACL
    * ROOP
    * Alma-0

Fourth Generation Commercial Environment Programming Languages

    * FOCUS
    * MARK-IV
    * Oracle Express 4GL
    * SAS
    * XML mosaic
    * Aubit 4GL
    * CorVision
    * Uniface
    * LINC 4GL
    * ABAP
    * Ubercode
    * xBase
    * MAPPER
    * Visual DataFlex
    * Today
    * Visual FoxPro

Machine Programming Languages

    * UltraSPARC
    * Motorola 6800
    * Intel 8008/8080/8085
    * StrongARM
    * ARM
    * Commodore 64 CPU
    * MIPS R2000/ R3000
    * National 32032

Interactive Mode Programming Languages

    * BASIC
    * Clojure
    * CLisp
    * Erlang
    * F#
    * Forth
    * FPr
    * Fril
    * Haskell
    * IDL
    * Lua
    * MUMPS
    * Maple
    * MATLAB
    * ML
    * Mythryl
    * Perl
    * PostScript
    * Python
    * R
    * REXX
    * Ruby
    * Scala
    * Scheme
    * SmallTalk
    * S-Lang
    * Tcl
    * Windows PowerShell

Interpreted Programming Languages

    * Ant
    * APL
    * AutoHotkey
    * Autolt
    * BASIC
    * Databus
    * Eiffel
    * Forth
    * FPr
    * Frink
    * GML
    * Groovy
    * Haskell
    * J
    * LISP
    * LPC
    * Lua
    * Lush
    * MUMPS
    * Maple
    * Pascal
    * Perl
    * Pikt
    * PostScript
    * Python
    * REXX
    * R
    * Ruby
    * S-Lang
    * Spin
    * TorqueScript
    * thinBasic
    * VBScript
    * Windowes PowerShell
    * XMLmosaic

Iterative Programming Languages

    * Aldor
    * Alphard
    * C#
    * CLU
    * Cobra
    * Eiffel
    * Icon
    * IPL-v
    * Lua
    * Lush
    * Python
    * Sather
    * XL

List Based Programming Languages

    * FPr
    * Joy
    * Lisp
    * Lush
    * R
    * TCl
    * TRAC

Little Languages

    * apply
    * awk
    * Comet
    * SQL

Macro Programming Languages

    * cpp
    * m4
    * PHP
    * SMX

Meta programming Languages

    * C++
    * Curl
    * D
    * Forth
    * Haskell
    * Lisp
    * Lua
    * Maude System
    * MetaL
    * MetaOCaml
    * Nemerle
    * Perl
    * Python
    * ruby
    * SmallTalk
    * XL

Multiparadigm Programming Languages

    * Ada
    * ALF
    * Alma
    * APL
    * BETA
    * C++
    * C#
    * ChucK
    * Cobra
    * CLisp
    * Corn
    * Curl
    * Curry
    * D
    * Delphi
    * Dylan
    * ECMAScript
    * Eiffel
    * F
    * Fantom
    * FPr
    * Harbour
    * Hop
    * J
    * LabVIEW
    * Lasso
    * Lava
    * Leda
    * Lua
    * Metaobject protocols
    * Mythryl
    * Nemerle
    * Objective Camrl
    * Oz
    * Object Pascal
    * Perl
    * PHP
    * Pliant
    * Poplog
    * ppC++
    * Prograph
    * Python
    * R
    * REBOL
    * ROOP
    * Ruby
    * Scala
    * Seed 7
    * SISAL
    * Spreadsheets
    * Tcl
    * Windows PowerShell
    * XL

Numerical Analysis Programming Languages

    * Algae
    * AMPL
    * GAMS
    * MATLAB
    * Seneca

Non-English Based Programming Languages

    * ARLOGO
    * Chinese BASIC
    * Fjölnir
    * HPL
    * Lexico
    * Rapira
    * Glagol
    * Portugol

Object Oriented Class Based Programming Languages

    * CLisp
    * Dylan
    * Goo
    * Cecil
    * Actor
    * Ada 95
    * BETA
    * C++
    * Chrome
    * ChucK
    * Cobra
    * ColdFusion
    * Corn
    * Curl
    * D
    * DASL
    * Delphi
    * dylan.NET
    * E
    * Ecere C
    * Eiffel
    * F-Script
    * Fortran
    * Fortress
    * FPr
    * GAMBAS
    * GML
    * Harbour
    * j
    * Java
    * Kite
    * LabVIEW
    * Lava
    * Lua
    * Modula-2
    * Moto
    * Nemerle
    * NetRexx
    * Oberon-2
    * Pbject Pascal
    * Object Caml
    * Perl 5
    * PHP
    * Pliant
    * ppC++
    * Prograph
    * Python
    * Revolution
    * Ruby
    * Scala
    * Seccia
    * Simula
    * SmallTalk
    * SPIN
    * SuperCollider
    * VBScript
    * Visual DataFlex
    * Visual FoxPro
    * Visual Prolog
    * X++
    * XOTcl

Object Oriented Prototype Based Programming Languages

    * ABCL/1/R/R2/c plus
    * Agora
    * cecil
    * ECMAScript
    * Etoys
    * Glyphic script
    * Io
    * Lisaac
    * Lua
    * MOO
    * NewtonScript
    * Obliq
    * R
    * REBOL
    * Self
    * Slate
    * TADS

OFFSide Rule Programming Languages

    * ISWIM
    * ABC
    * Hyper Talk
    * Ivy
    * Miranda
    * Occam
    * Pliant
    * SPIn
    * XL

Procedural Programming Languages

    * Ada
    * ALGOL
    * Alma-0
    * BASIC
    * BLISS
    * C
    * C++
    * C#
    * ChucK
    * Cobra
    * COBOL
    * ColdFusion
    * Component Pascal
    * Curl
    * D
    * DASL
    * dylan.NET
    * Delphi
    * Ecere C
    * ECMAScript
    * Eiffel
    * Fortran
    * FPC Pascal
    * Harbour
    * Hyper Talk
    * Java
    * JOVIAL
    * Lasso
    * Modula-2
    * Oberon
    * Oberon-2
    * MATLAB
    * MUMPS
    * Nemerle
    * Occam
    * Pascal
    * PCASTL
    * Perl
    * PL/C
    * PL/I
    * Plus
    * Python
    * R
    * Rapira
    * RPG
    * S-Lang
    * VBScript
    * Visual Basic
    * Visual FoxPro
    * X++
    * XL
    * XMLmosaic

Reflective Languages

    * Aspect Oriented
    * Befunge
    * C##
    * ChucK
    * Cobra
    * Component Pascal Black Box Component Builder
    * Cobra
    * Curl
    * DSelphi
    * ECMAScript
    * Eiffel
    * Forth
    * Harbour
    * Java
    * Lisp
    * Lua
    * Maude System
    * .NET FCLR
    * Oberon-2
    * Objective-C
    * PCASTL
    * Perl
    * PHP
    * Pico
    * Pliant
    * Poplog
    * Prolog
    * Python
    * REBOL
    * Ruby
    * SmallTalk
    * SNOBOL
    * Tcl
    * X++
    * XL

Rule Based Programming Languages

    * CLIPS
    * Constraint Handling Rules
    * Jess
    * OPS5
    * Prolog
    * Poses++

Scripting Languages

    * AppleScript
    * AWK
    * BeanShell
    * Ch
    * CLIST
    * ColdFusion
    * ECMAScript
    * CMS EXEC
    * EXEC 2
    * F-Script
    * Falcon
    * Frink
    * GML
    * ICI
    * Io
    * JASS
    * Groovy
    * Join Java
    * Tea
    * Lua
    * MEL
    * Mythryl
    * Perl
    * PHP
    * Pikt
    * Python
    * R
    * REBOL
    * REXX
    * Revolution
    * Ruby
    * SmallTalk
    * S-Lang
    * Se
    * Tcl
    * TorqueScript
    * VBScript
    * Windows PowerShell
    * Winbatch

Stack Based Programming Languages

    * Cat
    * colorForth
    * Factor
    * Forth
    * Joy
    * Piet
    * Poplog
    * PostScript
    * RPL
    * Urq

Synchronous Programming Languages

    * Argos
    * Averest
    * Esterel
    * LEA
    * Lustre
    * Signal
    * SynchCharts

Syntax Handling Programming Languages

    * ANTLR
    * Coco/R
    * GNU bison
    * GNU Flex
    * Lex
    * M4
    * yacc
    * JavaCC
    * Rats!

Visual Programming Languages

    * CODE
    * Eiffel
    * Fabrik
    * LabVIEW
    * Lava
    * Limnor
    * Mindscript
    * Max
    * NXT-G
    * PPL
    * Prograph
    * Pure Data
    * Quartz Composer
    * Scratch
    * Simulink
    * Spreadsheets
    * Subtext
    * Tinkertoy
    * VEE
    * VisSim
    * ww
    * EICASLAB

Niklaus Wirth Programming Languages

    * ALGOL W
    * Modula
    * Modula-2 (Obliq based on Modula 3)
    * Oberon
    * Oberon-2
    * Oberon-07
    * Object Pascal

XML Based Programming Languages

    * Ant
    * C Omega
    * Jelly
    * LZX
    * MXML
    * XQuery
    * XSLT
    * XMLmosaic

It is common for many programming languages to fall under multiple categories based upon their structure, function, orientation or any other criteria. Computer programming languages' popularity depends upon these versatilities and multi-functionalities.

Latest Computer Programming Languages

The following programming languages were developed in the years starting from 2000. Let's take a look at this list of the most recently developed programming languages.

    * Alma-0
    * Aspect-J
    * Ada 2005
    * Boo
    * C#
    * Cobra
    * Clojure
    * D
    * F#
    * Fantom
    * Factor
    * Ferite
    * Groovy
    * Go
    * Io
    * Joy
    * Join Java
    * Kite
    * Links
    * Little b
    * Nemerle
    * OptimJ
    * Oberon-07
    * Pure
    * Squirrel
    * Scala
    * Subtext
    * Visual Basic .Net
    * Vala
    * Windows PowerShell
    * XL

# ActorScript – theoretical purely actor-based language defined in terms of itself
# Ada
# Afnix – concurrent access to data is protected automatically (previously called Aleph, but unrelated to Alef)
# Alef – concurrent language with threads and message passing, used for systems programming in early versions of Plan 9 from Bell Labs
# Alice – extension to Standard ML, adds support for concurrency via futures.
# Ateji PX – an extension to Java with parallel primitives inspired from pi-calculus
# Axum – domain specific concurrent programming language, based on the Actor model and on the .NET Common Language Runtime using a C-like syntax.
# Chapel – a parallel programming language being developed by Cray Inc.
# Charm++ – C++-like language for thousands of processors.
# Cilk – a concurrent C
# Cω – C Omega, a research language extending C#, uses asynchronous communication
# Clojure – a modern Lisp targeting the JVM
# Concurrent Clean – a functional programming language, similar to Haskell
# Concurrent Haskell – lazy, pure functional language operating concurrent processes on shared memory
# Concurrent ML – a concurrent extension of Standard ML
# Concurrent Pascal – by Per Brinch Hansen
# Curry
# E – uses promises, ensures deadlocks cannot occur
# Eiffel – through its SCOOP mechanism based on the concepts of Design by Contract
# Erlang – uses asynchronous message passing with nothing shared
# Faust – Realtime functional programming language for signal processing. The Faust compiler provides automatic parallelization using either OpenMP or a specific work-stealing scheduler.
# Go – systems programming language with explicit support for concurrent programming
# Io – actor-based concurrency
# Janus features distinct "askers" and "tellers" to logical variables, bag channels; is purely declarative
# JoCaml
# Join Java – concurrent language based on the Java programming language
# Joule – dataflow language, communicates by message passing
# Joyce – a concurrent teaching language built on Concurrent Pascal with features from CSP by Per Brinch Hansen
# LabVIEW – graphical, dataflow programming language, in which functions are nodes in a graph and data is wires between those nodes. Includes object oriented language extensions.
# Limbo – relative of Alef, used for systems programming in Inferno (operating system)
# MultiLisp – Scheme variant extended to support parallelism
# Modula-3 – modern language in Algol family with extensive support for threads, mutexes, condition variables.
# Newsqueak – research language with channels as first-class values; predecessor of Alef
# occam – influenced heavily by Communicating Sequential Processes (CSP).

    * occam-π – a modern variant of occam, which incorporates ideas from Milner's π-calculus

# Orc – a heavily concurrent, nondeterministic language based on Kleene algebra.
# Oz – multiparadigm language, supports shared-state and message-passing concurrency, and futures

    * Mozart Programming System – multiplatform Oz

# Pict – essentially an executable implementation of Milner's π-calculus
# Perl with AnyEvent and Coro
# Python with greenlet and gevent.
# Reia – uses asynchronous message passing between shared-nothing objects
# SALSA – actor language with token-passing, join, and first-class continuations for distributed computing over the Internet
# Scala – a general purpose programming language designed to express common programming patterns in a concise, elegant, and type-safe way
# SR – research language
# Stackless Python
# SuperPascal – a concurrent teaching language built on Concurrent Pascal and Joyce by Per Brinch Hansen
# Unicon – Research language.
# Termite Scheme adds Erlang-like concurrency to Scheme
# TNSDL – a language used at developing telecommunication exchanges, uses asynchronous message passing
# VHDL – VHSIC Hardware Description Language, aka IEEE STD-1076
# XC – a concurrency-extended subset of the C programming language developed by XMOS based on Communicating Sequential Processes. The language also offers built-in constructs for programmable I/O.

*)
