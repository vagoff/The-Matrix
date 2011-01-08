(* data schema *)

con id = int (* do not use directly! *)

con num = int
con smallint = int
con title = string
con caption = string
con desc = string
con text = string
con markdown = string
con serialized _ = string
con lang_id = id
con feature_id = id
con view_id = id
con feature_group_id = id
con user_id = id
con time_stamp = time
con host_addr = string
con short_str = string
con long_str = string

table lang_def :
    { LangId : lang_id
    , Caption : caption (* PHP53, MSVB5 *)
    , Title : title (* PHP 5.3.0, Microsoft Visual Basic 5.0 *)
    , Desc : option desc (* Abstract *)
    , Home : option url
    } PRIMARY KEY LangId
    , CONSTRAINT Caption UNIQUE Caption
    , CONSTRAINT Title UNIQUE Title
    , CONSTRAINT Home UNIQUE Home

type enum_type =
    { VerbatimValue : list short_str
    , SortOrder : smallint
    }

datatype feature_type'
    = Enum of enum_type
    | TimeStamp
    | URL of url * option title
    | LongStr
    | ShortStr
    | ShortInt

type feature_type = list1 feature_type' (* 1-n tuple of simpler types *)

datatype feature_value'
    = EnumVal of smallint
    | TimeVal of timestamp
    | URLVal of url
    | MarkdownVal of markdown
    | ShortStrVal of short_str
    | LongStrVal of long_str
    | ShortIntVal of short_int

type feature_value = list1 feature_value'

table feature_def :
    { FeatureId : feature_id
    , Caption : caption
    , Title : title
    , Desc : option desc
    , Home : option url
    , TypeDef : serialized feature_type
    } PRIMARY KEY FeatureId
    , CONSTRAINT Caption UNIQUE Caption
    , CONSTRAINT Title UNIQUE Title

table info :
    { LangId : lang_id
    , FeatureId : feature_id
    , Value : serialized feature_value
    , Link : option url (* prooflink :) *)
    , Remark : option desc (* popups in hint *)
    , Code : option markdown
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
