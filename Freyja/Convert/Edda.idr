-- ---------------------------------------------------------------- [ Edda.idr ]
-- Module    : Edda.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Freyja.Convert.Edda

import GRL.Lang.GLang

import Edda
import Edda.Refine
import Edda.Writer.Org

import Freyja

-- ------------------------------------------------------------------- [ Utils ]

sectionTemplate : Nat -> Maybe String -> EddaString -> Edda PRIME BLOCK
sectionTemplate n l t = Section PRIME n l t Nil

section : Maybe String -> EddaString -> Edda PRIME BLOCK
section l t = sectionTemplate 0 l t

subsection : Maybe String -> EddaString -> Edda PRIME BLOCK
subsection l t = sectionTemplate 1 l t

subsubsection : Maybe String -> EddaString -> Edda PRIME BLOCK
subsubsection l t = sectionTemplate 2 l t

subsubsubsection : Maybe String -> EddaString -> Edda PRIME BLOCK
subsubsubsection l t = sectionTemplate 3 l t

-- ----------------------------------------------------------------- [ Affects ]

convertAffect : Affect -> EddaBody
convertAffect (MkAffect c r d) =
      [subsubsubsection Nothing
        ((Emph [Text $ show c]) :: Space :: name r ) ]
   ++ fromMaybe Nil d

-- ------------------------------------------------------------------ [ Traits ]

convertTrait : Trait ty -> EddaBody
convertTrait (MkTrait ty n d s as) =
      [subsubsection Nothing (Text (show ty) :: Colon :: Space :: n)]
   ++ d
   ++ [DList PRIME
         [MkPair [Text "Satisfaction"] [Text (show s)]]]
   ++ concatMap convertAffect as

convertTraits : DList TTy Trait ts -> EddaBody
convertTraits Nil     = Nil
convertTraits (m::ms) = convertTrait m ++ convertTraits ms

-- -------------------------------------------------------------- [ Properties ]

convertProperty : Property -> EddaBody
convertProperty (MkProperty n d ts) =
      [subsection Nothing (Text "Property" :: Colon :: Space :: n)]
   ++ d
   ++ convertTraits ts

-- ------------------------------------------------------------------ [ Models ]

convertModel : Model ty -> EddaBody
convertModel (MkModel n ty d m) =
      [subsection Nothing (Text (show ty) :: Colon :: Space :: n)]
   ++ [Listing
         Nothing
         (Text (show ty) :: Colon :: Space :: n)
         Nothing
         Nothing
         Nil
         m]
   ++ d

convertModels : DList MTy Model ms -> EddaBody
convertModels Nil     = Nil
convertModels (m::ms) = convertModel m ++ convertModels ms

-- ---------------------------------------------------------------- [ Solution ]

convertSolution : Solution -> EddaBody
convertSolution (MkSolution n d ms ps) =
       [section Nothing (Text "Solution" :: Colon :: Space :: n)]
    ++ d
    ++ convertModels ms
    ++ concatMap convertProperty ps

-- ------------------------------------------------------------ [ Requirements ]

convertReq : Requirement ty -> EddaBody
convertReq (MkReq ty n d) =
    [ subsection Nothing
        (Text (show ty) :: Colon :: Space :: n)]
    ++ d


convertReqs : DList RTy Requirement ms -> EddaBody
convertReqs Nil     = Nil
convertReqs (m::ms) = convertReq m ++ convertReqs ms

-- ----------------------------------------------------------------- [ Problem ]

convertProblem : Problem -> EddaBody
convertProblem (MkProblem n d rs) =
    [section Nothing (Text "Problem" :: Colon :: Space :: n)]
    ++
    d
    ++
    [subsection Nothing [Text "Requirements"]]
    ++ convertReqs rs


-- ----------------------------------------------------------------- [ Context ]
convertDomain : Context -> EddaBody
convertDomain (MkContext t d) = tblock :: d
  where
    tblock : Edda PRIME BLOCK
    tblock = section Nothing (Text "Context" :: Colon :: Space :: t)

-- ---------------------------------------------------------------- [ MetaData ]

convertMetadata : Metadata -> EddaBody
convertMetadata mdata =
       [section Nothing [Text "Metadata"]]
    ++ [subsection Nothing [Text "Aliases"]]
    ++ [BList (aliases mdata)]
    ++ [subsection Nothing [Text "Tags"]]
    ++ [BList (tags mdata)]
    ++ [subsection Nothing [Text "Authors"]]
    ++ [BList (authors mdata)]
    ++ [subsection Nothing [Text "Auditors"]]
    ++ [BList (auditors mdata)]
    ++ [subsection Nothing [Text "Times"]]
    ++ [DList PRIME
          [ MkPair [Text "Created"]  [Text (created mdata)]
          , MkPair [Text "Modified"] [Text (modified mdata)]
          , MkPair [Text "Evaluted"] [Text (evaluated mdata)]
          , MkPair [Text "ID"]       [Text (ident mdata)]]
       ]

-- ---------------------------------------------------------------- [ Evidence ]

convertEvidence : EddaBody -> EddaBody
convertEvidence d = [section Nothing [Text "Evidence"]] ++ d

-- ----------------------------------------------------------------- [ Studies ]

convertStudies : List Study -> EddaBody
convertStudies ss = [section Nothing [Text "Case Studies"]]
    ++ concatMap convertStudy ss
  where
    convertStudy : Study -> EddaBody
    convertStudy (MkStudy n b a) =
        [subsection Nothing (Text "Study" :: Colon :: Space :: n)]
     ++ [subsubsection Nothing [Text "Before"]]
     ++ b
     ++ [subsection Nothing [Text "Study"]]
     ++ a

-- --------------------------------------------------------------- [ Relations ]

convertRelations : DList LTy Relation ls -> EddaBody
convertRelations _ = Nil -- @TODO

-- ----------------------------------------------------------------- [ Pattern ]

convertPattern : PatternDoc -> Edda PRIME MODEL
convertPattern p = MkEdda as (intersperse (Empty PRIME) body)
  where
    as : List (String, String)
    as = [MkPair "TITLE" (inlines $ name p)]

    body : EddaBody
    body = summary p
        ++ (convertDomain $ context p)
        ++ (convertProblem $ problem p)
        ++ (convertSolution $ solution p)
        ++ (convertEvidence $ evidence p)
        ++ (convertStudies $ studies p)
        ++ (convertRelations $ relations p)
        ++ (convertMetadata $ mdata p)


public
toEdda : PatternDoc -> Edda PRIME MODEL
toEdda pdoc = (convertPattern pdoc)

-- --------------------------------------------------------------------- [ EOF ]
