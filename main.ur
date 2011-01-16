open Base
val fixme : transaction page = return <xml>fixme</xml>
con list0 = List0.list0
con list1 = List1.list1
type vec = Oset.oset
structure V = Oset
structure M = Matrix
open Schema
open Fixme

(* select * from (select * from a,b order by a.a,b.b) as ab left join rel on (ab.a=rel.a and ab.b=rel.b); *)

(*
fun loadCellDataAccordingToTheView () = 
*)

fun loadRawCellData =
	(ls,fs,m) <- query
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

	return
		{ Langs = V.keys ls
		, Feats = V.keys fs
		, FeatsSorting = Unsorted
		, LangsSorting = Unsorted
		, Transposed = false
		, DeletedLangs = V.empty
		, DeletedFeats = V.empty
		, LangData = ls
		, FeatData = fs
		, CellData = m
		}

datatype sort_order = Ascending | Descending

fun invertSortOrder o =
	case o of
		Ascending => Descending
	  | Descending => Ascending

datatype sorting a = Unsorted | Sorted of (sort_order, a, option a)

fun moveSortMode col =
	case mode of
		Unsorted => Sorted (col,None)
	  | Sorted (prev,_) => Sorted (new,Some prev)

fun sort dir vec by =
	let
		fun by' =
			case d of
				Ascending => by
			  | Descending => fn a b => invertOrder (by a b)
	in
		V.sortBy by' vec
	end

type view_state =
	{ Langs : vec lang_id
	, Feats : vec feature_id
	, FeatsSorting : sorting lang_id
	, LangsSorting : sorting feature_id
	, Transposed : bool (* true = langs on left, features on top; false = vice versa *)
	, DeletedLangs : list0 lang_id
	, DeletedFeats : list0 feature_id
	, LangData : dict lang_id lang_data
	, FeatData : dict feature_id feature_data
	, CellData : dict (lang_id * feature_id) cell_data
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
	{ Pos : pos
	, Buttons : mouse_buttons_state
	}

type mouse_event =
	{ Pos : pos
	, Type : mouse_event_type
	}

datatype dnd_stage = Idle | Starting | Dragging

type dnd_state a =
	{ Stage : dnd_stage
	, Pos0 : pos
	, SavedState : a
	}

type dragging_state a =
	{ Lmb : dnd_state a
	, Rmb : dnd_state a
	}

(* local helper *)
datatype lang_id_or_feature_id = LID of lang_id | FID of feature_id

fun buildMainPage =
	let

	val cfg =
		{ DragThresholdPixels = 3
		, DoubleClickMilliseconds = 50
		, DraggingZIndex = 5
		}

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

	fun dndEventProcessor ev ds =
		let
			fun dndStateMachine ds btn =
				case (ds.Stage,btn) of
					(Idle,MouseDown) => rset State Starting (rset Pos0 ev.Pos0 ds)
				  | (Starting,MouseUp) => performClick ds
				  | (Starting,MouseMove) =>
						if abs (ds.Pos0.Left - pos.Left) >= cfg.DragThresholdPixels
						|| abs (ds.Pos0.Top - pos.Top) >= cfg.DragThresholdPixels
						then
							rset Stage Dragging ds
						else
							ds
				  | (Dragging,CancelDragging) => cancelDragging (rset Stage Idle ds)
				  | (Dragging,MouseUp) => completeDragging ds
				  | _ => ds
		in
			rset Pos0 ev.Pos0 (dndStateMachine ds)
		end


	fun updateField r nm f = rupd nm f r (* [!] to lib? *)

	fun reorderLangs vs =
		case st.LangsSorting of
			Unsorted => vs
		  | Sorted (dir,fid1,mb_fid2) =>
				updateField vs Langs (sort dir st.Langs (fn lid1 lid2 =>
					case compareCellsAt (lid1,fid1) (lid2,fid1) st.CellData of
						EQ =>
							case mb_fid2 of
								None => EQ
							  | Some fid2 => compareCellsAt (lid1,fid2) (lid2,fid2) vs.CellData
					  | LT => LT
					  | GT => GT))

	fun reorderFeats st =
		case st.FeatsSorting of
			Unsorted => st
		  | Sorted (dir,lid1,mb_lid2) =>
				updateField st Feats (sort dir st.Feats (fn fid1 fid2 =>
					case compareCellsAt (lid1,fid1) (lid1,fid2) st.CellData of
						EQ =>
							case mb_lid2 of
								None => EQ
							  | Some lid2 => compareCellsAt (lid2,fid1) (lid2,fid2) st.CellData
					  | LT => LT
					  | GT => GT))

	fun sendCancelDragging =
		dndEventProcessor CancelDragging st

	fun onMouseEvent id =
		pos <- locateMouse;
		w <- currentMouseButtons;
		whichButtons w;
		st <- return (processButton Left dndEventProcessor dndSt);
		st <- return (processButton Right dndEventProcessor dndSt);
		if btns.Middle then sendCancelDragging;
		st <- return (processButton Left dndEventProcessor dndSt);
		dndEventProcessor st id btn

	(rowid,colid) <- return if vs.Transposed then (LID lid, FID fid) else (FID fid, LID lid)
	
    fun sideImage alt url proc = <xml><img {Src=url, Alt=alt, OnClick=proc}/></xml>

    fun horiz_layout = mapX fn item => <xml><table><tr><td>{item}</td></tr></table></xml>

    (* pressXXX are user input handlers *)    

    pressCaption = return ()
    pressCell = return ()

    (* renderXXX are signals *)
    
	fun renderCell cellId vs : signal xbody =
	    return <xml>
            <td onmouseup={pressCell rowid colid}
                onmousedown={pressCell rowid colid}
                onmouseover={pressCell rowid colid
            >
  		        {st.}
	    	</td>
	    </xml>

    fun renderCaption (id : lang_id_or_feature_id) (vs : view_state) : signal xbody =
        let
            renderCaption' idstr (caption,hint) =
                return <xml>
                    <td onmouseup={pressCaption id}
                        onmousedown={pressCaption id}
                        onmouseover={pressCaption id}
                    >
                        {[caption]}({[hint]})
                    </td> (* [!] hint as hint *)
                </xml>
        in    
            case id of
                LID lid => renderCaption' (toString lid) let data = fetch vs.LangData lid in (data.Caption,data.Title) end
              | FID fid => renderCaption' (toString fid) let data = fetch vs.FeatData.fid in (data.Caption,data.Title) end
        end

	fun renderTop (vsRef,dsRef) : signal xbody =
	    vs <- signal vsRef;
    	cap <-
    	    mapMX renderCaption
    		    (if vs.Transposed then
    	    		(map LID (V.toList vs.Langs))
    		    else
	    		    (map FID (V.toList vs.Feats)));
		return <xml><tr>{cap}</tr></xml>

	fun renderBody (vsRef,dsRef) : signal xbody =
	    vs <- signal vsRef;
   		if st.Transposed then
   			mapX (fn fid =>
   			        cap <- renderCaption (LID lid) vs;
   				    row <-
   				        mapX (fn lid =>
   				                renderCell (lid,fid) vs)
           				    (V.toList vs.Langs);
           		    return <xml><tr>{cap}{row}</tr></xml>)
   	    	    (V.toList st.Feats)
   		else
   			mapX (fn lid =>
   			        cap <- renderCaption (FID fid) vs;
   				    row <-
   				        mapX (fn fid =>
   				                renderCell (lid,fid) vs)
           				    (V.toList vs.Feats);
           		    return <xml><tr>{cap}{row}</tr></xml>)
   	    	    (V.toList vs.Langs)

    fun mouseOutDoc = sendCancelDragging

    fun withSt f stRef =
        st <- get stRef
        set stRef (f st)

	in
    	vs <- loadRawCellData;
    	vsRef <- source vs;
    	dsRef <- source initialDndState;
    	return
    		<xml
    			<body onmouseout={mouseOutDoc (vsRef,dsRef)}>
    				{sideImage "feed1" "please give us feedback" "contact_us.png" Feedback.feedback}
    				{sideImage "feed2" "please help us improve this site" "feedback.gif" Feedback.feedback}
    				{horiz_layout
    					(  <xml><button value="save view as..." onclick={fixme}></xml>
    					:: <xml><button value="new attribute" onclick={fixme}></xml>
    					:: <xml><button value="new language" onclick={fixme}></xml>
    					:: <xml><button value="transpose views" onclick={fixme}></xml>
    					:: <xml><button value="top views" onclick={fixme}></xml>
    					::[])}
    				<table>
    					<dyn {Signal=renderTop (vsRef,dsRef)}/>
    					<dyn {Signal=renderBody (vsRef,dsRef)}/>
    				</table>
    			</body>
    		</xml>
    end
