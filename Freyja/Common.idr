-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Freya.Common

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

readRTy : String -> Maybe RTy
readRTy s =
  case s of
    "functional"     => Just FUNC
    "usability"      => Just USAB
    "reliability"    => Just RELI
    "performance"    => Just PERF
    "supportability" => Just SUPP
    otherwise        => Nothing

data TTy = ADV  | DIS | GEN

instance Show TTy where
  show ADV = "ADV"
  show DIS = "DIS"
  show GEN = "GEN"

instance Cast TTy String where
  cast ADV = "advantage"
  cast DIS = "disadvantage"
  cast GEN = "general"

readTTy : String -> Maybe TTy
readTTy s =
  case s of
    "advantage"    => Just ADV
    "disadvantage" => Just DIS
    "general"      => Just GEN
    otherwise      => Nothing

data MTy = STRUCT | DYN

instance Show MTy where
  show STRUCT = "STRUCT"
  show DYN    = "DYN"

instance Cast MTy String where
  cast STRUCT = "structure"
  cast DYN    = "dynamic"

readMTy : String -> Maybe MTy
readMTy s =
  case s of
    "structure" => Just STRUCT
    "dynamic"   => Just DYN
    otherwise   => Nothing

data LTy = SPECIAL | IMPL | USES | LINK

instance Show LTy where
  show SPECIAL = "SPECIAL"
  show IMPL    = "IMPL"
  show USES    = "USES"
  show LINK    = "LINK"

instance Cast LTy String where
  cast SPECIAL = "specialises"
  cast IMPL    = "implements"
  cast USES    = "requires"
  cast LINK    = "linked"

readLTy : String -> Maybe LTy
readLTy s =
  case s of
    "specialises" => Just SPECIAL
    "implements"  => Just IMPL
    "requires"    => Just USES
    "linked"      => Just LINK
    otherwise     => Nothing


-- --------------------------------------------------------------------- [ EOF ]
