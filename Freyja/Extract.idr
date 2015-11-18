-- ------------------------------------------------------------- [ Extract.idr ]
-- Module    : Extract.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Freyja.Extract

import Data.Sigma.DList
import GRL.Lang.GLang

import Edda
import Edda.Reader.Org

import XML.DOM
import XML.Reader
import XML.XPath

import Freyja

import public Freyja.Error

import Freyja.Fetch
import Freyja.Utils

import Debug.Trace

-- ---------------------------------------------------------------- [ MetaData ]

metadata : XMLDoc -> Extract Metadata
metadata doc = do
  as    <- getEddaStrings doc ("/pattern/metadata/aliases/alias")
  ts    <- getEddaStrings doc ("/pattern/metadata/tags/tag")
  auths <- getEddaStrings doc ("/pattern/metadata/authors/author")
  es    <- getEddaStrings doc ("/pattern/metadata/auditors/auditor")
  c     <- getTextNode  doc ("/pattern/metadata/created")
  m     <- getTextNode  doc ("/pattern/metadata/modified")
  e     <- getTextNode  doc ("/pattern/metadata/evaluated")
  t     <- getNamedAttr doc "id" ("/pattern")

  pure $ MkMData as ts auths es c m e t

-- ------------------------------------------------------------ [ Requirements ]

requirement : (ty : RTy) -> XMLNode -> Extract (Sigma RTy Requirement)
requirement ty (Node e@(Element _ _ _)) = do
  n <- getEddaString e "name"
  d <- getEddaBlock  e "description"
  pure $ (ty ** MkReq ty n d)
requirement _ _ = Left $ GeneralError "error"

requirements : XMLDoc -> Extract (ts ** DList RTy Requirement ts)
requirements doc = do
  fs <- getNodes doc "/pattern/problem/requirements/functional"
  us <- getNodes doc "/pattern/problem/requirements/usability"
  rs <- getNodes doc "/pattern/problem/requirements/reliability"
  ps <- getNodes doc "/pattern/problem/requirements/performance"
  ss <- getNodes doc "/pattern/problem/requirements/supportability"

  fs' <- mapEither (\x => requirement FUNC x) fs
  us' <- mapEither (\x => requirement USAB x) us
  rs' <- mapEither (\x => requirement RELI x) rs
  ps' <- mapEither (\x => requirement PERF x) ps
  ss' <- mapEither (\x => requirement SUPP x) ss

  let res = fs' ++ us' ++ rs' ++ ps' ++ ss'

  pure $ DList.fromLDP res

-- ----------------------------------------------------------------- [ Problem ]

problem : XMLDoc -> Extract Problem
problem doc = do
  rs <- requirements doc
  n <- getEddaString doc "/pattern/problem/name"
  d <- getEddaBlock  doc "/pattern/problem/description"
  pure $ MkProblem n d (getProof rs)

-- ----------------------------------------------------------------- [ Context ]

context : XMLDoc -> Extract Context
context doc = do
  n <- getEddaString doc "/pattern/context/name"
  d <- getEddaBlock  doc "/pattern/context/description"
  pure $ MkContext n d

-- ----------------------------------------------------------------- [ Affects ]

tryGetReq : XMLElem -> Maybe (Sigma RTy Requirement)
tryGetReq x = do
  naam <- getNodeName x
  ty   <- readRTy naam
  case requirement ty (Node x) of
    Left err => Nothing
    Right r  => pure r

getReqByID : String -> List XMLNode -> Extract (Sigma RTy Requirement)
getReqByID ival ns =
    case foldl doFind Nothing ns of
      Nothing => Left $ GeneralError ("Requirement not found.")
      Just r  => pure r
  where
    doFind : Maybe (Sigma RTy Requirement)
          -> XMLNode
          -> Maybe (Sigma RTy Requirement)
    doFind res (Node e@(Element _ _ _)) =
      case getAttribute "id" e of
        Nothing => res
        Just y  =>
          if ival == y
            then tryGetReq e
            else res
    doFind res _ = res

affect : XMLDoc -> XMLNode -> Extract Affect
affect doc (Node e@(Element _ _ _)) = do
  c <- getNamedAttr e "cvalue" "//affect"
  l <- getNamedAttr e "id" "//affect"
  rs <- getNodes doc "/pattern/problem/requirements/*"
  (_ ** r)  <- getReqByID l rs
  let d = case getEddaBlock e "affect" of
            Left err  => Nothing
            Right res => Just res
  pure $ MkAffect (cast c) r d
affect _ _ = Left $ GeneralError "error"

-- ------------------------------------------------------------------ [ Traits ]

trait : XMLDoc -> TTy -> XMLNode -> Extract (Sigma TTy Trait)
trait doc ty (Node e@(Element _ _ _)) = do
  n   <- getEddaString e "//name"
  d   <- getEddaBlock  e "//description"
  s   <- getNamedAttr  e "svalue" (with List concat ["//", cast ty ])
  as  <- getNodes      e "//affects/affect"
  as' <- mapEither (\x => affect doc x) as
  pure $ (ty ** MkTrait ty n d (cast s) as')
trait _ _ _ = Left $ GeneralError "error"

-- ---------------------------------------------------------------- [ Property ]

property : XMLDoc -> XMLNode -> Extract Property
property doc (Node e@(Element _ _ _)) = do
  n  <- getEddaString e "/property/name"
  d  <- getEddaBlock  e "/property/description"

  as <- getNodes e "/property/traits/advantage"
  ds <- getNodes e "/property/traits/disadvantage"
  gs <- getNodes e "/property/traits/general"

  as' <- mapEither (\x => trait doc ADV x) as
  ds' <- mapEither (\x => trait doc DIS x) ds
  gs' <- mapEither (\x => trait doc GEN x) gs

  let ts' = as' ++ ds' ++ gs'

  pure $ MkProperty n d (getProof $ DList.fromLDP ts')
property _ _ = Left $ GeneralError "error"

properties : XMLDoc -> Extract (List Property)
properties doc = do
  ps  <- getNodes doc "/pattern/solution/properties/property"
  ps' <- mapEither (\p => property doc p) ps
  pure ps'

-- ------------------------------------------------------------------ [ Models ]

model : MTy -> XMLNode -> Extract (Sigma MTy Model)
model ty (Node e@(Element _ _ _)) = do
  n <- getEddaString e "//name"
  d <- getEddaBlock  e "//description"
  m <- getCDataNode  e "//model"

  pure $ (ty ** MkModel n ty d m)
model _ _ = Left $ GeneralError "error"


models : XMLDoc -> Extract (ms ** DList MTy Model ms)
models doc = do
  ss <- getNodes doc "/pattern/solution/models/structure"
  ds <- getNodes doc "/pattern/solution/models/dynamic"

  ss' <- mapEither (\x => model STRUCT x) ss
  ds' <- mapEither (\x => model DYN x)    ds

  let res = ss' ++ ds'

  pure $ DList.fromLDP res

-- ---------------------------------------------------------------- [ Solution ]

solution : XMLDoc -> Extract Solution
solution doc = do
  n <- getEddaString doc "/pattern/solution/name"
  d <- getEddaBlock  doc "/pattern/solution/description"

  ms <- models doc
  ps <- properties doc

  pure $ MkSolution n d (getProof ms) ps

-- ----------------------------------------------------------------- [ Studies ]

study : XMLNode -> Extract Study
study (Node e@(Element _ _ _)) = do
  n <- getEddaString e "/study/name"
  b <- getEddaBlock e "/study/before"
  a <- getEddaBlock e "/study/after"
  pure $ MkStudy n b a
study _ = Left $ GeneralError "Error"


studies : XMLDoc -> Extract (List Study)
studies doc = do
  ss <- getNodes doc "/pattern/studies/study"
  ss' <- mapEither study ss
  pure $ ss'


-- --------------------------------------------------------------- [ Relations ]

getMaybeDesc : XMLElem -> Maybe EddaBody
getMaybeDesc n = do
    naam <- getNodeName n
    case getEddaBlock n naam of
        Left  err => Nothing
        Right res => Just res

relation : XMLNode -> Extract (Sigma LTy Relation)
relation (Node e@(Element _ _ _)) = do
    ty <- extractTy e
    f <- getNamedAttr e "patternID" (with List concat ["//", cast ty ])
    let d = getMaybeDesc e
    pure $ (ty ** MkRelation ty f d)
  where
    getTy : XMLElem -> Maybe LTy
    getTy x = do
        naam <- getNodeName x
        ty   <- readLTy naam
        pure ty
    extractTy : XMLElem -> Extract LTy
    extractTy x =
      case getTy x of
        Nothing => Left $ GeneralError "Error"
        Just n  => pure $ n

relation _ = Left $ GeneralError "Error"

relations : XMLDoc -> Extract (ls ** DList LTy Relation ls)
relations doc = do
    ls <- getNodes doc "/pattern/relations/*"
    ls' <- mapEither relation ls
    pure $ DList.fromLDP ls'

-- ---------------------------------------------------------------- [ Document ]

document : XMLDoc -> Extract PatternDoc
document doc = do
  n <- getEddaString doc "/pattern/name"
  d <- getEddaBlock  doc "/pattern/description"
  e <- getEddaBlock  doc "/pattern/evidence"
  m <- metadata doc
  c <- context  doc
  p <- problem  doc
  s <- solution doc
  ss <- studies doc
  rs <- relations doc
  pure $ MkPDoc n d m c p s e ss (getProof rs)

-- --------------------------------------------------------------------- [ EOF ]
