module Freyja.Convert.XML

import Effects
import Effect.State

import Data.AVL.Dict
import Data.Sigma.DList
import Data.Sigma.DList.Eff

import Edda
import Edda.Writer.Org

import GRL.Lang.GLang

import XML.DOM

import Freyja.Types

-- -------------------------------------------------------------- [ Directives ]

%access private
%default partial

-- ------------------------------------------------------------------- [ Utils ]

{-
mkPCNode : String -> String -> Document ELEMENT
mkPCNode p c = mkNode p <++> (c <+=> "TO BE DETERMIND")

mkEmptyNode : String -> Document ELEMENT
mkEmptyNode s = (s <+=> "TO BE DETERMIND")

addScore : Document ELEMENT -> Document ELEMENT
addScore = setAttribute "score" "TO BE DETERMINED"

mkDescNode : Maybe String -> Document ELEMENT
mkDescNode Nothing  = "description" <+=> "TO BE DETERMINED"
mkDescNode (Just d) = "description" <+=> d

mkNDNode : String -> String -> Maybe String -> Document ELEMENT
mkNDNode n t d = (addScore $ mkNode n)
            <++> (addScore $ mkDescNode d)
            <++> (addScore $ "name" <+=> t)
-}

mkNameDescNode : String
              -> EddaString
              -> EddaBody
              -> XMLElem
mkNameDescNode p n d = mkNode p
    <++> ("name" <+=> inlines n)
    <++> ("description" <+=> concatMap block d)

convertMany : String
           -> String
           -> (a -> String)
           -> List a
           -> XMLElem
convertMany p c f ns = foldl doFold (mkNode p) ns
  where
    doFold : XMLElem -> a -> XMLElem
    doFold p t = p <++> (c <+=> f t)

convertEList : String -> String -> List EddaString -> XMLElem
convertEList o i cs = convertMany o i inlines cs


-- ---------------------------------------------------------------- [ Metadata ]

convertMData : Metadata -> XMLElem
convertMData mdata = setAttribute "id" (ident mdata) $
        mkNode "metadata"
  <++> (convertEList "auditors" "auditor" (auditors mdata))
  <++> (convertEList "tags"     "tag"     (tags mdata))
  <++> (convertEList "authors"  "author"  (authors mdata))
  <++> (convertEList "auditors" "auditor" (auditors mdata))
  <++> ("evaluated" <+=> (evaluated mdata))
  <++> ("modified"  <+=> (modified mdata))
  <++> ("modified"  <+=> (created mdata))

convertStudies : List Study -> Document ELEMENT
convertStudies = foldl (mkStudy) (mkNode "studies")
  where
    mkStudy : XMLElem -> Study -> XMLElem
    mkStudy p (MkStudy n b a) = p <++> (
        mkNode "study"
      <++> ("name"   <+=> inlines n)
      <++> ("before" <+=> concatMap block b)
      <++> ("after"  <+=> concatMap block a))

convertModel : Model ty -> XMLElem
convertModel (MkModel n ty d m) =
         setAttribute "modelTy" "unknown" $
         mkNode (tname ty)
    <++> ("name" <+=> inlines n)
    <++> ("description" <+=> concatMap block d)
    <++> (mkNode "model" <++> CData m)

  where
    tname : MTy -> String
    tname STRUCT = "structure"
    tname DYN    = "dynamic"

convertModels : DList MTy Model ms -> XMLElem
convertModels = DList.foldl (doConvert) (mkNode "models")
  where
    doConvert : XMLElem -> Model ty -> XMLElem
    doConvert p m = p <++> (convertModel m)

convertContext : Context -> XMLElem
convertContext (MkContext n d) = mkNameDescNode "context" n d
-- @TODO
-- mkRels : Document ELEMENT
-- mkRels = addScore $ mkNode "relations"
--     <++> (setAttribute "patternID" "TO BE DETERMINED" $
--             setAttribute "relationship" "specialises | implements | uses | linkedTo" (mkNode "link"))


-- --------------------------------------------------------------------- [ XML ]

XEffs : List EFFECT
XEffs = [STATE (Nat, Dict String Nat)]

convertReq : Requirement ty -> Eff (XMLElem) XEffs
convertReq (MkReq ty n d) = do
    let r = mkNameDescNode (cast ty) n d
    (idGen,_) <- get
    let idVal = cast {to=Int} (S idGen)
    let e = setAttribute "id" (cast idVal) r
    update (\(idGen,ids) => ((S idGen), insert (inlines n) (S idGen) ids))
    pure $ e

convertProblem : Problem -> Eff (XMLElem) XEffs
convertProblem (MkProblem n d rs) = do
       let e = mkNameDescNode "problem" n d
       rsC <-  mapDListE (\r => convertReq r) rs
       let rNodes = Foldable.foldl
                       (\n,r => n <++> r)
                       (mkNode "requirements")
                       rsC
       pure $ e <++> rNodes

convertAffect : Affect -> Eff (XMLElem) XEffs
convertAffect (MkAffect c r d) = do
       (_,ids) <- get
       let id = Dict.lookup (inlines $ name r) ids
       let e = case d of
           Nothing => mkNode "affect"
           Just xs => "affect" <+=> concatMap block xs
       let idval = cast {to=Int} $ fromMaybe 0 id
       pure $ (setAttribute "cvalue" (cast c)
              (setAttribute "linksTo" (cast idval) e))

convertTrait : Trait ty -> Eff (XMLElem) XEffs
convertTrait (MkTrait ty n d s as) = do
       let e  = mkNameDescNode (toLower $ show ty) n d
       let e' = setAttribute "svalue" (cast s) e
       -- <mess>
       asC <- mapE (\a => convertAffect a) as
       let rNodes = foldl (\n,r => n <++> r) (mkNode "affects") asC
       -- </mess>
       pure $ e' <++> (rNodes)

convertProperty : Property -> Eff (XMLElem) XEffs
convertProperty (MkProperty n d ts) = do
       let e = mkNameDescNode "property" n d
       -- <mess>
       tsC <- mapDListE (\t => convertTrait t) ts
       let rNodes = foldl (\n,r => n <++> r) (mkNode "traits") tsC
       -- </mess>
       pure $ e <++> rNodes

convertSolution : Solution -> Eff (XMLElem) XEffs
convertSolution (MkSolution n d ms ps) = do
       let e = mkNameDescNode "solution" n d
       -- <mess>
       psC <- mapE (\p => convertProperty p) ps
       let rNodes = foldl (\n,r => n <++> r) (mkNode "properties") psC
       -- </mess>
       let mNodes = convertModels ms

       pure $ e <++> rNodes <++> mNodes

convertPattern : PatternDoc -> Eff (XMLElem) XEffs
convertPattern p = do
    pnode <- convertProblem (problem p)
    snode <- convertSolution (solution p)
    pure $ mkNode "pattern"
        -- <++> mkRels
       <++> (convertStudies (studies p))
       <++> ("evidence" <+=> concatMap block (evidence p))
       <++> snode
       <++> pnode
       <++> (convertContext (context p))
       <++> (convertMData (mdata p))
       <++> ("description" <+=> concatMap block (summary p))
       <++> ("name" <+=> inlines (name p))

public
toXML : PatternDoc -> XMLDoc
toXML p =  mkDocument root
  where
    partial
    root : XMLElem
    root = runPureInit [(Z,Dict.empty)] (convertPattern p)


-- --------------------------------------------------------------------- [ EOF ]
