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

type format = Export.format

fun main () = Main.buildMainPage
fun views viewId = SavedView.views fiewId
fun exports format = Export.exports format
fun search queryString = Search.search queryString
