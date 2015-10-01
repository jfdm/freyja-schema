-- ------------------------------------------------------------- [ Convert.idr ]
-- Module    : Convert.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Freyja.Convert

import XML.DOM

import Edda
import Edda.Writer.Org
import Edda.Writer.LaTeX
import Edda.Writer.CommonMark

import Freyja
import Freyja.Convert.XML
import Freyja.Convert.Edda

data FreyjaOutFormat = ORG | LATEX | CMARK | XML | EDDA

instance Eq FreyjaOutFormat where
  (==) LATEX   LATEX   = True
  (==) CMARK   CMARK   = True
  (==) ORG     ORG     = True
  (==) XML     XML     = True
  (==) EDDA    EDDA    = True
  (==) _       _       = False

instance Show FreyjaOutFormat where
  show LATEX = "LaTeX"
  show CMARK = "CommonMark"
  show ORG   = "Org"
  show XML   = "XML"
  show EDDA  = "EDDA"

readOutFMT : String -> Maybe FreyjaOutFormat
readOutFMT s =
  case toLower s of
    "latex"    => Just LATEX
    "markdown" => Just CMARK
    "org"      => Just ORG
    "xml"      => Just XML
    otherwise  => Nothing

convTy : FreyjaOutFormat -> Type
convTy LATEX   = String
convTy CMARK   = String
convTy ORG     = String
convTy XML     = XMLDoc
convTy EDDA    = EddaDoc

convTo : (fmt : FreyjaOutFormat) -> PatternDoc-> (convTy fmt)
convTo XML   p = toXML p
convTo EDDA  p = toEdda p
convTo ORG   p = org $ toEdda p
convTo LATEX p = latex $ toEdda p
convTo CMARK p = markdown $ toEdda p

-- --------------------------------------------------------------------- [ EOF ]
