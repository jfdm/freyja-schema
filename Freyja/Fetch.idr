-- --------------------------------------------------------------- [ Query.idr ]
-- Module    : Query.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Freyja.Fetch

import Edda
import Edda.Reader.Org

import XML.DOM
import XML.XPath
import XML.XPath.Query

import Freyja
import Freyja.Error
import Freyja.Utils

%access export
-- ----------------------------------------------------------------- [ Queries ]

getNodes : (node : Document ty)
        -> {auto prf : CanQuery ty}
        -> (qstr : String)
        -> Extract (List XMLNode)
getNodes doc qstr = Query.getNodes ExtractionError qstr doc

getNode : (node : Document ty)
       -> {auto prf : CanQuery ty}
       -> (qstr : String)
       -> Extract XMLNode
getNode doc qstr = Query.getNode ExtractionError qstr doc

getNamedAttrs : (node : Document ty)
             -> {auto prf : CanQuery ty}
             -> (name : String)
             -> (qstr : String)
             -> Extract (List String)
getNamedAttrs doc name qstr = Query.getNamedAttrs name ExtractionError qstr doc


getNamedAttr : (node : Document ty)
            -> {auto prf : CanQuery ty}
            -> (name : String)
            -> (qstr : String)
            -> Extract String
getNamedAttr doc name qstr = Query.getNamedAttr name ExtractionError qstr doc


getTextNodes : (node : Document ty)
            -> {auto prf : CanQuery ty}
            -> (qstr : String)
            -> Extract (List String)
getTextNodes doc qstr = Query.getTextNodes ExtractionError qstr doc

getTextNode : (node : Document ty)
           -> {auto prf : CanQuery ty}
           -> (qstr : String)
           -> Extract String
getTextNode doc qstr = Query.getTextNode ExtractionError qstr doc

getCDataNode : (node : Document ty)
            -> {auto prf : CanQuery ty}
            -> (qstr : String)
            -> Extract String
getCDataNode doc qstr = Query.getCDataNode ExtractionError qstr doc

-- -------------------------------------------------- [ Markup Related Queries ]

getEddaStrings : Document ty
             -> {auto prf : CanQuery ty}
             -> String
             -> Extract (List EddaString)
getEddaStrings e qstr = do
    res <- getTextNodes ExtractionError qstr e
    ss  <- mapEither getString res
    pure ss
  where
    getString : String -> Extract $ EddaString
    getString s =
      case readOrgInline s of
        Left err  => Left $ TextConvError err
        Right str => Right str

getEddaString : Document ty
             -> {auto prf : CanQuery ty}
             -> String
             -> Extract EddaString
getEddaString e qstr = do
  res <- getTextNode ExtractionError qstr e
  case readOrgInline res of
    Left err  => Left $ TextConvError err
    Right str => Right str


getEddaBlock : Document ty
            -> {auto prf : CanQuery ty}
            -> String
            -> Extract EddaBody
getEddaBlock e qstr = do
  res <- getTextNode ExtractionError qstr e
  case readOrgBody res of
    Left err  => Left $ TextConvError err
    Right str => Right str

-- --------------------------------------------------------------------- [ EOF ]
