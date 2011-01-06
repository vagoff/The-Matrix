(* open Base *)

datatype exports = YAML | XML | CVS | SXML


(* todo:
 - call languages all lowercase?
 - how to cope with languages with duplicate names?!
 - add reference to comprehensive documentation for every feature (!)
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
    , Caption : caption
    , Title : title
    , Home : option url
    , Authors : option
    , Desc : option desc
    , LicenseName : option desc
    , LicenseText : option desc
    , LastRealeaseDate : time_stamp
    } PRIMARY KEY LangId
    , CONSTRAINT Caption UNIQUE Caption
    , CONSTRAINT Title UNIQUE Title
    , CONSTRAINT Home UNIQUE Home

(* [!] "date of last patch in repository" *)
(* [!] "list of backends" *)

table feature_def :
    { FeatureId : feature_id
    , Caption : caption
    , Title : title
    , Home : option url
    , Desc : option desc
    , Predefined : list0 string
    , ExtraNumberAllowed : bool
    } PRIMARY KEY FeatureId
    , CONSTRAINT Caption UNIQUE Caption
    , CONSTRAINT Title UNIQUE Title

table info :
    { LangId : lang_id
    , FeatureId : feature_id
    , Value : smallint
    , Link : option url
    , Remark : option desc
    , Code : option markdown
    , ExtraNumber : option float
    } CONSTRAINT Addr UNIQUE (LangId, FeatureId)
    , CONSTRAINT LangId FOREIGN KEY LangId REFERENCES lang_def(LangId)
    , CONSTRAINT FeatureId FOREIGN KEY FeatureId REFERENCES feature_def(FeatureId)

table saved_view_lang :
    { ViewId : view_id
    , Order : num
    , LangId : lang_id
    }

table saved_view_feature :
    { ViewId : view_id
    , Order : num
    , FeatureId : feature_id
    }

table saved_view :
    { ViewId : view_id
    , UserId : user_id
    , Time : time_stamp
    , Title : title
    , Desc : desc
    , Transposed : bool (* false: features at top, langs at left, true: langs at top, features at left *)
    }

table interesting_langs :
    { UserId : user_id
    , Order : num
    , LangId : lang_id
    , InterestLevel : smallint
    }

table users :
    { UserId : user_id
    , Title : title
    , Desc : desc
    } PRIMARY KEY UserId
    , CONSTRAINT Title UNIQUE Title

(* [!] Created : time_stamp *)

(*

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

*)

fun fixme () = return <xml>fixme</xml>

fun main () = return <xml></xml>

fun views viewId = fixme ()

fun exports format = fixme ()

funl search queryString = fixme ()
