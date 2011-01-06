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

con id = int
con num = int
con title = string
con desc = string
con text = string
con serialized = string
con attr_type_id = id

con a = list int

con lang_id = id
con attr_id = id

con entry :: {Type} =
    [ Titl = title
    , Desc = desc
    ] 

con langs =
    [ Id = lang_id
    , Title = title
    , Desc = desc
    ]

con attrs =
    [ Id = attr_id
    ]

con cell =
    [ Link = option url
    , Remark = option desc
    , Code = text
    , Typ = attr_type_id
    , ValId = id
    , Val = serialized
    ]

con attr_list =
    [ No = num
    , Id = attr_id
    ]

con lang_list =
    [ No = num
    , Id = lang_id
    ]




fun loadMatrix () =
    matrix <- source (M.new (?,?))
    data <- query
	(SELECT
	    al.attr_id, 
	    cells LEFT JOIN  ON (lang_id = AND ))
    set matrix data

	    sideImage "please give us feedback" "contactus.png" feedback
	    sideImage "please help us improve this site" "feedback.gif" feedback

	    <button value="save view as..." onclick={fixme}>
	    <button value="new attribute" onclick={fixme}>
	    <button value="new language" onclick={fixme}>
	    <button value="transpose views" onclick={fixme}>
	    <button value="top views" onclick={fixme}>
    
<td onmouseup={withTopSide coln (ButtonUp LeftButton)}> (* [!] todo: which button? *)
<td onmousedown={withTopSide coln (ButtonDown LeftButton)}> (* [!] todo: which button? *)
<td onmouseover={withTopSide coln (MoveOver (0,0))}> (* [!] todo: which coords? *)

<td onmouseup={withLeftSide rown (ButtonUp LeftButton)}> (* [!] todo: which button? *)
<td onmousedown={withLeftSide rown (ButtonDown LeftButton)}> (* [!] todo: which button? *)
<td onmouseover={withLeftSide rown (MoveOver (0,0))}> (* [!] todo: which coords? *)

fun fixme () = return <xml>fixme</xml>

fun main () = return <xml></xml>

fun view viewId = fixme ()

fun export format = fixme ()

funl search queryString = fixme ()
