-- -------------------------------------------------------------- [ Freyja.idr ]
-- Module    : Freyja.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Core Data Types.
module Freyja

import public Data.DList

import public GRL.Lang.GLang

import public Edda

import public Freyja.Common

%access public export
-- ----------------------------------------------------------- [ Element Types ]

-- ---------------------------------------------------------------- [ MetaData ]

record Metadata where
  constructor MkMData
  aliases   : List EddaString
  tags      : List EddaString
  authors   : List EddaString
  auditors  : List EddaString
  created   : String
  modified  : String
  evaluated : String
  ident     : String

implementation Show Metadata where
  show (MkMData as bs cs ds e f g i) = unwords
    [ "MkMData", show as, show bs, show cs, show ds
    , show e, show f, show g, show i]

-- ------------------------------------------------------------ [ Requirements ]

record Requirement (x : RTy) where
  constructor MkReq
  ty   : RTy
  name : EddaString
  desc : EddaBody

implementation Show (Requirement ty) where
  show (MkReq typ n d) = unwords
    [ "MkReq", show typ, show n, show d]

-- ---------------------------------------------------------------- [ Problems ]

record Problem where
  constructor MkProblem
  name : EddaString
  desc : EddaBody
  reqs : DList RTy Requirement rs

implementation Show Problem where
  show (MkProblem n d rs) = unwords
    ["MkProblem", show n, show d, showDList show rs]


-- ------------------------------------------------------------------ [ Models ]

record Model (x : MTy) where
  constructor MkModel
  name  : EddaString
  ty    : MTy
  desc  : EddaBody
  model : String

implementation Show (Model ty) where
  show (MkModel n typ d m) = unwords
    ["MkModel", show n, show typ, show d, show m]

-- ----------------------------------------------------------------- [ Affects ]

record Affect where
  constructor MkAffect
  cval : CValue
  req  : Requirement ty
  desc : Maybe (EddaBody)

implementation Show Affect where
  show (MkAffect c r d) = unwords
    ["MkAffect", show c, show r, show d]

-- ------------------------------------------------------------------ [ Traits ]

record Trait (x : TTy) where
  constructor MkTrait
  ty : TTy
  name : EddaString
  desc : EddaBody
  sval : SValue
  affects : List Affect

implementation Show (Trait ty) where
  show (MkTrait typ n d s as) = unwords
    ["MkTrait", show typ, show n, show d, show s, show as]

-- ---------------------------------------------------------------- [ Property ]

record Property where
  constructor MkProperty
  name   : EddaString
  desc   : EddaBody
  traits : DList TTy Trait ts

implementation Show (Property) where
  show (MkProperty n d ts) = unwords
    ["MkProperty", show n, show d, showDList show ts]

-- ---------------------------------------------------------------- [ Solution ]

record Solution where
  constructor MkSolution
  name       : EddaString
  desc       : EddaBody
  models     : DList MTy Model ms
  properties : List Property

implementation Show Solution where
  show (MkSolution n d ms ps) = unwords
    ["MkSolution", show n, show d, showDList show ms, show ps]

-- ----------------------------------------------------------------- [ Context ]

record Context where
  constructor MkContext
  name : EddaString
  desc : EddaBody

implementation Show Context where
  show (MkContext n d) = unwords
    ["MkContext", show n, show d]

-- ------------------------------------------------------------------- [ Study ]

record Study where
  constructor MkStudy
  name : EddaString
  before : EddaBody
  after  : EddaBody

implementation Show Study where
  show (MkStudy n b a) = unwords
    ["MkStudy", show b, show b, show a]

-- ---------------------------------------------------------------- [ Relation ]

record Relation (x : LTy) where
  constructor MkRelation
  ty    : LTy
  ident : String
  desc  : Maybe EddaBody

implementation Show (Relation ty) where
  show (MkRelation typ i d) = unwords
    ["MkRelation", show typ, show i, show d]

-- ------------------------------------------------------------- [ Pattern Doc ]

record PatternDoc where
  constructor MkPDoc
  name      : EddaString
  summary   : EddaBody
  mdata     : Metadata
  context   : Context
  problem   : Problem
  solution  : Solution
  evidence  : EddaBody
  studies   : List Study
  relations : DList LTy Relation ls

implementation Show PatternDoc where
  show (MkPDoc n d m c p s e ss rs) = unwords
      [ "MkPDoc"
      , show n, show d, show m, show c
      , show p, show s, show e, show ss
      , showDList show rs]

-- --------------------------------------------------------------------- [ EOF ]
