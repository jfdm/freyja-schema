-- --------------------------------------------------------------- [ Types.idr ]
-- Module    : Types.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Convenience Wrappers for returning information from XML queries.
module Freyja.Types

import Data.Sigma.DList

import GRL.Lang.GLang

import Edda

-- ----------------------------------------------------------- [ Element Types ]

namespace Freyja

  data RTy = FUNC | USAB | RELI | PERF | SUPP

  instance Cast RTy String where
    cast FUNC = "functional"
    cast USAB = "usability"
    cast RELI = "reliability"
    cast PERF = "performance"
    cast SUPP = "supportability"

  instance Show RTy where
    show FUNC = "FUNC"
    show USAB = "USAB"
    show RELI = "RELI"
    show PERF = "PERF"
    show SUPP = "SUPP"

  data TTy = ADV  | DIS | GEN

  instance Show TTy where
    show ADV = "ADV"
    show DIS = "DIS"
    show GEN = "GEN"

  data MTy = STRUCT | DYN

  instance Show MTy where
    show STRUCT = "STRUCT"
    show DYN    = "DYN"

  data LTy = SPECIAL | IMPL | USES | LINK

  instance Show LTy where
    show SPECIAL = "SPECIAL"
    show IMPL    = "IMPL"
    show USES    = "USES"
    show LINK    = "LINK"

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

  instance Show Metadata where
    show (MkMData as bs cs ds e f g i) = unwords
      [ "MkMData", show as, show bs, show cs, show ds
      , show e, show f, show g, show i]

-- ------------------------------------------------------------ [ Requirements ]

  record Requirement (x : RTy) where
    constructor MkReq
    ty   : RTy
    name : EddaString
    desc : EddaBody

  instance Show (Requirement ty) where
    show (MkReq ty n d) = unwords
      [ "MkReq", show ty, show n, show d]

-- ---------------------------------------------------------------- [ Problems ]

  record Problem where
    constructor MkProblem
    name : EddaString
    desc : EddaBody
    reqs : DList RTy Requirement rs

  instance Show Problem where
    show (MkProblem n d rs) = unwords
      ["MkProblem", show n, show d, showDList show rs]


-- ------------------------------------------------------------------ [ Models ]

  record Model (x : MTy) where
    constructor MkModel
    name  : EddaString
    ty    : MTy
    desc  : EddaBody
    model : String

  instance Show (Model ty) where
    show (MkModel n ty d m) = unwords
      ["MkModel", show n, show ty, show d, show m]

-- ----------------------------------------------------------------- [ Affects ]

  record Affect where
    constructor MkAffect
    cval : CValue
    req  : Requirement ty
    desc : Maybe (EddaBody)

  instance Show Affect where
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

  instance Show (Trait ty) where
    show (MkTrait ty n d s as) = unwords
      ["MkTrait", show ty, show n, show d, show s, show as]

-- ---------------------------------------------------------------- [ Property ]

  record Property where
    constructor MkProperty
    name   : EddaString
    desc   : EddaBody
    traits : DList TTy Trait ts

  instance Show (Property) where
    show (MkProperty n d ts) = unwords
      ["MkProperty", show n, show d, showDList show ts]

-- ---------------------------------------------------------------- [ Solution ]

  record Solution where
    constructor MkSolution
    name       : EddaString
    desc       : EddaBody
    models     : DList MTy Model ms
    properties : List Property

  instance Show Solution where
    show (MkSolution n d ms ps) = unwords
      ["MkSolution", show n, show d, showDList show ms, show ps]

-- ----------------------------------------------------------------- [ Context ]

  record Context where
    constructor MkContext
    name : EddaString
    desc : EddaBody

  instance Show Context where
    show (MkContext n d) = unwords
      ["MkContext", show n, show d]

-- ------------------------------------------------------------------- [ Study ]

  record Study where
    constructor MkStudy
    name : EddaString
    before : EddaBody
    after  : EddaBody

  instance Show Study where
    show (MkStudy n b a) = unwords
      ["MkStudy", show b, show b, show a]

-- ---------------------------------------------------------------- [ Relation ]

  record Relation (x : LTy) where
    constructor MkRelation
    ty    : LTy
    ident : String
    desc  : Relation ty

  instance Show (Relation ty) where
    show (MkRelation ty i d) = unwords
      ["MkRelation", show ty, show i, show d]

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

  instance Show PatternDoc where
    show (MkPDoc n d m c p s e ss rs) = unwords
        [ "MkPDoc"
        , show n, show d, show m, show c
        , show p, show s, show e, show ss
        , showDList show rs]

-- --------------------------------------------------------------------- [ EOF ]
