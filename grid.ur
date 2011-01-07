(* open Base *)

con serialized a = string

datatype format = YAML | XML | CVS | SXML

(* todo:
 - call languages all lowercase?
 - how to cope with languages with duplicate names?!
 - add reference to comprehensive documentation for every feature (!)
 - values as flag set
 - reCAPTCHA

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
 - Please, no references to Wikipedia! It is not source of ultimate truth but just current public opinion.
   Even if some article in Wikipedia is perfect on the time you referenced it, it may become worse over time!
   links to university programming language courses, google knols, Stanford's Plato, links to lecture notes in programming,
   etc are very appreciated.
   - has single distinct author (to whom you may send wrathy emails)
   - author has some considerable background in subject
*)

open Listbox
structure M = Matrix

structure Mouse = struct
    type xCoord = int
    type yCoord = int
    datatype button = LeftButton | RightButton | MiddleButton
    datatype event = ButtonUp of button | ButtonDown of button | MoveOver of (xCoord,yCoord)
end
open Mouse

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

con id = int (* do not use directly! *)

con num = int
con smallint = int
con title = string
con caption = string
con desc = string
con text = string
con markdown = string
con serialized = string
con lang_id = id
con feature_id = id

table lang_def :
    { LangId : lang_id
    , Caption : caption (* PHP53, MSVB5 *)
    , Title : title (* PHP 5.3.0, Microsoft Visual Basic 5.0 *)
    , Desc : option desc (* Abstract *)
    , Home : option url
    , Authors : option
    , LicenseName : option desc
    , LicenseText : option desc
    , LastRealeaseDate : time_stamp
    } PRIMARY KEY LangId
    , CONSTRAINT Caption UNIQUE Caption
    , CONSTRAINT Title UNIQUE Title
    , CONSTRAINT Home UNIQUE Home

table feature_def :
    { FeatureId : feature_id
    , Caption : caption
    , Title : title
    , Desc : option desc
    , Home : option url
    , Predefined : list0 string
    , ExtraNumberAllowed : bool
    } PRIMARY KEY FeatureId
    , CONSTRAINT Caption UNIQUE Caption
    , CONSTRAINT Title UNIQUE Title

table info :
    { LangId : lang_id
    , FeatureId : feature_id
    , Value : smallint (* serialized value ? *)
    , Link : option url
    , Remark : option desc
    , Code : option markdown
    , ExtraNumber : option float
    } CONSTRAINT Addr UNIQUE (LangId, FeatureId)
    , CONSTRAINT LangId FOREIGN KEY LangId REFERENCES lang_def(LangId)
    , CONSTRAINT FeatureId FOREIGN KEY FeatureId REFERENCES feature_def(FeatureId)

table feature_group :
    { FeatureGroupId : feature_group_id
    , FeatureId : feature_id
    , Caption : caption
    , Title : title
    , Desc : option desc
    } PRIMARY KEY FeatureGroupId
    , CONSTRAINT Caption UNIQUE Caption
    , CONSTRAINT Title UNIQUE Title

table saved_view_lang :
    { ViewId : view_id
    , Order : num
    , LangId : lang_id
    } CONSTRAINT Rel UNIQUE (ViewId,LangId)

table saved_view_feature :
    { ViewId : view_id
    , Order : num
    , FeatureId : feature_id
    } CONSTRAINT Rel UNIQUE (ViewId,FeatureId)

table saved_view :
    { ViewId : view_id
    , UserId : user_id
    , Time : time_stamp
    , Title : title
    , Desc : desc
    , Transposed : bool (* false: features at top, langs at left, true: langs at top, features at left *)
    } PRIMARY KEY ViewId

table interesting_langs :
    { UserId : user_id
    , Order : num
    , LangId : lang_id
    , InterestLevel : smallint
    } CONSTRAINT Rel UNIQUE (UserId,LangId)

table user :
    { UserId : user_id
    , Title : title
    , Desc : option desc (* about *)
    , Home : option url (* homepage or blog *)
    } PRIMARY KEY UserId
    , CONSTRAINT Title UNIQUE Title

(*val fieldsOf (fields ::: {Type}) (tbl :: $fields) 

datatype transaction
    = EditLang of (option lang_def,lang_def)
    | EditFeature of (option feature_def,feature_def)
    | EditInfo of (option info,info)
    | EditUser of (option user,user)
    | SaveView of (option (saved_view,saved_view_lang,saved_view_feature),(saved_view,saved_view_lang,saved_view_feature))
    | EditInterest of (option interesting_langs, interesting_langs)

table transaction_log :
    { TransId : trans_id
    , UserId : user_id (* who *)
    , TimeStamp : time_stamp (* when *)
    , HostAddr : host_addr (* where *)
    , TransData : serialized transaction (* what *)
    }
*)

fun feedback () = fixme

(* select * from (select * from a,b order by a.a,b.b) as ab left join rel on (ab.a=rel.a and ab.b=rel.b); *)

(*
fun loadMatrixAccordingToTheView () = 
*)

fun loadEntireMatrix () =
    m <- source (M.new (?,?))
    data <- query
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
		, M.set (r.Lid,r.Fid) dval m
		)
	    end)
	(V.empty, V.empty, M.empty)
    set matrix (Some m)

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

invertSortOrder o =
    case o of
	Ascending => Descending
      | Descending => Ascending

datatype dir = Ascending of id | Descending of id
datatype sort = Unsorted | SortByOne of dir | SortByTwo of (dir,dir)
datatype state
    = Direct of (vec lang_id * vec feature_id)
    | Transposed of (vec feature_id * vec lang_id)

type vec = Oset.oset

datatype touch_id = LID of lang_id | FID of feature_id

type vieww =
    { langs : vec lang_id
    , feats : vec feature_id
    , transposed : bool
    , deleted_langs : vec lang_id
    , deleted_feats : vec feature_id
    }

    fun touch

whichButtons w
     { Left = w % 2
     , Middle = (w / 2) % 2
     , Right = (w / 4) % 2
     }

<td onmouseup={touch TopSide coln (ButtonUp LeftButton)}>
<td onmousedown={touch TopSide coln (ButtonDown LeftButton)}>
<td onmouseover={touch TopSide coln (MoveOver (0,0))}>

<td onmouseup={touch LeftSide rown (ButtonUp LeftButton)}>
<td onmousedown={touch LeftSide rown (ButtonDown LeftButton)}>
<td onmouseover={touch LeftSide rown (MoveOver (0,0))}>

val fixme = return <xml>fixme</xml>

fun main () = return <xml></xml>

fun views viewId = fixme

fun exports format = fixme

fun search queryString = fixme
