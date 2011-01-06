(* open Base *)

open Listbox
structure M = Matrix

structure Mouse = struct
    type xCoord = int
    type yCoord = int
    datatype button = LeftButton | RightButton | MiddleButton
    datatype event = ButtonUp of button | ButtonDown of button | MoveOver (xCoord,yCoord)
end
open Mouse


(* primary concerns:
 - dynamically editable view
 - saved views
 - versioning and rollbacks
 - machine controlled attributes
 - code and urls in attrs
*)

(* on the second stage:
 - feedback forum like http://getsatisfaction.com (but open source and under MIT licence)
*)

(* buttons:
 - save view as ... (opens view manager)
 - new attr
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
con attr_id = id

table lang_def :
    { LangId : lang_id
    , Caption : caption
    , Title : title
    , Home : option url
    , Desc : option desc
    } PRIMARY KEY LangId
      CONSTRAINT Caption UNIQUE Caption
      CONSTRAINT Title UNIQUE Title
      CONSTRAINT Home UNIQUE Home

table attr_def :
    { AttrId : attr_id
    , Caption : caption
    , Title : title
    , Home : option url
    , Desc : option desc
    , Predefined : list0 string
    , ExtraNumberAllowed : bool
    } PRIMARY KEY AttrId
      CONSTRAINT Caption UNIQUE Caption
      CONSTRAINT Title UNIQUE Title

table info :
    { LangId : lang_id
    , AttrId : attr_id
    , Value : smallint
    , Link : option url
    , Remark : option desc
    , Code : option markdown
    , ExtraNumber : option float
    } CONSTRAINT Addr UNIQUE (LangId, AttrId)
      CONSTRAINT LangId FOREIGN KEY LangId REFERENCES lang_def(LangId)
      CONSTRAINT AttrId FOREIGN KEY AttrId REFERENCES attr_def(AttrId)

table saved_view_lang :
    { ViewId : view_id
    , Order : num
    , LangId : lang_id
    }

table saved_view_attr :
    { ViewId : view_id
    , Order : num
    , AttrId : attr_id
    }

table saved_view :
    { ViewId : view_id
    , UserId : user_id
    , Time : time_stamp
    , Title : title
    , Desc : desc
    , Transpose : bool (* false: attrs at top, langs at left, true: langs at top, attrs at left *)
    }

con attr_list =
    [ No = num
    , Id = attr_id
    ]

con lang_list =
    [ No = num
    , Id = lang_id
    ]


init
    val langs =
    val attrs =

fun loadMatrix () =
    matrix <- source (M.new (?,?))
    data <- query
	(SELECT
	    al.attr_id, 
	    cells LEFT JOIN  ON (lang_id = AND ))
    set matrix data

	    sideImage "feed1" "please give us feedback" "contactus.png" feedback
	    sideImage "feed2" "please help us improve this site" "feedback.gif" feedback

	    <button value="save view as..." onclick={fixme}>
	    <button value="new attribute" onclick={fixme}>
	    <button value="new language" onclick={fixme}>
	    <button value="transpose views" onclick={fixme}>
	    <button value="top views" onclick={fixme}>
    
<td onmouseup={touchTopSide coln (ButtonUp LeftButton)}> (* [!] todo: which button? *)
<td onmousedown={touchTopSide coln (ButtonDown LeftButton)}> (* [!] todo: which button? *)
<td onmouseover={touchTopSide coln (MoveOver (0,0))}> (* [!] todo: which coords? *)

<td onmouseup={touchLeftSide rown (ButtonUp LeftButton)}> (* [!] todo: which button? *)
<td onmousedown={touchLeftSide rown (ButtonDown LeftButton)}> (* [!] todo: which button? *)
<td onmouseover={touchLeftSide rown (MoveOver (0,0))}> (* [!] todo: which coords? *)

fun fixme () = return <xml>fixme</xml>

fun main () = return <xml></xml>

fun view viewId = fixme ()

fun export format = fixme ()

funl search queryString = fixme ()
