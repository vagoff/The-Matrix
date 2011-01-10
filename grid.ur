(* [!] move to the libs *)

open Base
val fixme : transaction page = return <xml>fixme</xml>
con list0 = List0.list0
con list1 = List1.list1
type vec = Oset.oset
structure V = Oset
structure M = Matrix
open Schema

datatype format = YAML | XML | CVS | SXML

(* todo:
 - call languages all lowercase?
 - how to cope with languages with duplicate names?!
 - add reference to comprehensive documentation for every feature (!)
 - values as flag set
 - reCAPTCHA
 - aliased features (e.g., promises = i-vars)

[!] Created : time_stamp
and versioning

lang def:
 - [!] "date of last patch in repository"
 - [!] "list of backends"

to decide:
 - it is need to allow small pictures in table cells?
*)

(* abbreviation rules:
 - desc, not descr
 - feature, not attr
*)

(* important notes:
 - work best for _any_ browser! (with gradual degradation if required) // very important!
 - all language attributes must be dynamic: stop mudding lang_def!
 - Please, no references to Wikipedia! It is not source of ultimate truth but just current public opinion.
   Even if some article in Wikipedia is perfect on the time you referenced it, it may become worse over time!
   links to university programming language courses, google knols, Stanford's Plato, links to lecture notes in programming,
   etc are very appreciated.
   - has single distinct author (to whom you may send wrathy emails)
   - author has some considerable background in subject
*)

(* primary concerns:
 - dynamically editable view
 - saved views
 - versioning and rollbacks
 - machine controlled features
 - code and urls in features
*)

(* on the second stage:
 - feedback forum like http://getsatisfaction.com (but open source and under MIT licence)
*)

(* buttons:
 - save view as ... (opens view manager)
 - new feature
 - new lang
 - look at view rate table (to select most popular view)
 - transpose view
and mouse to edit and reorder
*)

fun feedback () = fixme

(* select * from (select * from a,b order by a.a,b.b) as ab left join rel on (ab.a=rel.a and ab.b=rel.b); *)

(*
fun loadMatrixAccordingToTheView () = 
*)


fun loadEntireMatrix () =
	(lids,fids,m) <- query
		(SELECT
			lf.lid, lf.lcap, lf.ltit,
			lf.fid, lf.fcap, lf.ftit,
			d.value, d.link
		FROM
			(SELECT
				l.lang_id as lid,
				l.caption as lcap,
				l.title as ltit,
				f.feature_id as fid,
				f.caption as fcap,
				f.title as ftit
			FROM
				lang_def as l, feature_def as f
			ORDER BY
				l.lang_id, f.feature_id) AS lf
			LEFT JOIN info as d ON (lf.lang_id = d.lang_id AND lf.feature_id = d.feature_id))
		(fn r (l,f,m) =>
			let
				val lval = (r.Lcap, r.Ltit)
				val fval = (r.Fcap, r.Ftit)
				val dval = (d.value, d.link) 
			in
				( V.setIfNone r.Lid lval l
				, V.setIfNone r.Fid fval f
				, M.insert (r.Lid,r.Fid) dval m
				)
			end)
		(V.empty, V.empty, M.empty);

	state <- source
		{ Langs = lids
		, Feats = fids
		, FeatsSorting = Unsorted
		, LangsSorting = Unsorted
		, Transposed = false
		, DeletedLangs = V.empty
		, DeletedFeats = V.empty
		};

	sideImage "feed1" "please give us feedback" "contactus.png" feedback
	sideImage "feed2" "please help us improve this site" "feedback.gif" feedback

	horiz_layout
		[ <xml><button value="save view as..." onclick={fixme}></xml>
		, <xml><button value="new attribute" onclick={fixme}></xml>
		, <xml><button value="new language" onclick={fixme}></xml>
		, <xml><button value="transpose views" onclick={fixme}></xml>
		, <xml><button value="top views" onclick={fixme}></xml>
		]

datatype sort_order = Ascending | Descending

fun invertSortOrder o =
	case o of
		Ascending => Descending
	  | Descending => Ascending

datatype sorted a = Unsorted | Sorted of (sort_order, a, option a)

fun updateSortMode col =
	case mode of
		Unsorted => Sorted (col,None)
	  | Sorted (prev,_) => Sorted (new,Some prev)

datatype touch_id = LID of lang_id | FID of feature_id

type view_state =
	{ Langs : vec lang_id
	, Feats : vec feature_id
	, FeatsSorting : sorted lang_id
	, LangsSorting : sorted feature_id
	, Transposed : bool
	, DeletedLangs : vec lang_id
	, DeletedFeats : vec feature_id
	, Matrix : storage
	}

type coord = int
type pos = { Left : coord, Top : coord }
datatype mouse_event_type = MouseDown | MouseUp | MouseMove | CancelDragging
type mouse_buttons_state =
	{ LeftPressed : bool
	, RightPressed : bool
	, MiddlePressed : bool
	}
type mouse_state =
	{ pos : pos
	, buttons : mouse_buttons_state
	}
type mouse_event =
	{ Pos : pos
	, Type : mouse_event_type
	}

datatype dnd_stage = Idle | Starting | Dragging
type dnd_state =
	{ State : dnd_stage
	, Pos : pos
	}

type dragging_state =
	{ Lmb : dnd_state
	, Rmb : dnd_state
	}

	let

	val cfg =
		{ DragThresholdPixels = 3
		, DoubleClickMilliseconds = 50
		, DraggingZIndex = 5
		}

	fun dndEventProcessor ev =
		let
			fun dndStateMachine st btn =
				case (st.State,btn) of
					(Idle,MouseDown) => rset State Starting st
				  | (Starting,MouseUp) => performClick st
				  | (Starting,MouseMove) =>
						if abs (st.Pos.Left - pos.Left) >= cfg.DragThresholdPixels
						|| abs (st.Pos.Top - pos.Top) >= cfg.DragThresholdPixels
						then
							rset State Dragging st
						else
							st
				  | (Dragging,CancelDragging) => cancelDragging (rset State Idle st)
				  | (Dragging,MouseUp) => completeDragging st
				  | _ => st
		in
			rset Pos ev.Pos (dndStateMachine st)
		end

	fun sort dir vec by =
		let
			fun by' =
				case d of
					Ascending => by
				  | Descending => fn a b => invertOrder (by a b)
		in
			V.sortBy by' vec
		end

	fun updateField r nm f = rupd nm f r (* [!] to lib? *)

	fun reorderLangs st =
		case st.LangsSorting of
			Unsorted => st
		  | Sorted (dir,fid1,mb_fid2) =>
				updateField st Langs (sort dir st.Langs (fn lid1 lid2 =>
					case compareCellsAt (lid1,fid1) (lid2,fid1) st.Matrix of
						EQ =>
							case mb_fid2 of
								None => EQ
							  | Some fid2 => compareCellsAt (lid1,fid2) (lid2,fid2) st.Matrix
					  | LT => LT
					  | GT => GT))

	fun reorderFeats st =
		case st.FeatsSorting of
			Unsorted => st
		  | Sorted (dir,lid1,mb_lid2) =>
				updateField st Feats (sort dir st.Feats (fn fid1 fid2 =>
					case compareCellsAt (lid1,fid1) (lid1,fid2) st.Matrix of
						EQ =>
							case mb_lid2 of
								None => EQ
							  | Some lid2 => compareCellsAt (lid2,fid1) (lid2,fid2) st.Matrix
					  | LT => LT
					  | GT => GT))

	fun whichButtons w =
		{ Left = w % 2 <> 0
		, Middle = (w / 2) % 2 <> 0
		, Right = (w / 4) % 2 <> 0
		}

	fun locateMouse =
		x <- getCurrentMouseX
		y <- getCurrentMouseY
		return { Left = x, Top = y }

	fun processButton nm f st
		case (mst0.nm,mst1.nm) of
			(true,false) => f MouseUp st
		  | (false,true) => f MouseDown st
		  | _ => st

	fun sendCancelDragging =
		dndEventProcessor CancelDragging st

	fun onMouseEvent id =
		pos <- locateMouse
		w <- currentMouseButtons
		whichButtons w
		st <- return (processButton Left dndEventProcessor dndSt)
		st <- return (processButton Right dndEventProcessor dndSt)
		if btns.Middle then sendCancelDragging
		st <- return (processButton Left dndEventProcessor dndSt)
		dndEventProcessor st id btn

	(rowid,colid) <- return if vst.Transposed then (LID lid, FID fid) else (FID fid, LID lid)
	
	fun renderTop =
	
	fun renderBody =
	
	fun renderCell =

		mapX (fn cell =>
			<xml><td onclick={editCell cellAddr}>
			</xml>)
			(V.toList vec)

	in
	return <xml></xml>
	end

top
<td onmouseup={onMouseEvent colid}
    onmousedown={onMouseEvent colid}
    onmouseover={onMouseEvent colid>

left
<td onmouseup={onMouseEvent rowid}
    onmousedown={onMouseEvent rowid}
    onmouseover={onMouseEvent rowid}>

<body onmouseout={sendCancelDragging}>

val fixme = return <xml>fixme</xml>

fun main () = viewMatrix ()

fun views viewId = fixme

fun exports format = fixme

fun search queryString = fixme
