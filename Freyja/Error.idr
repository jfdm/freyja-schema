-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Freyja.Error

import XML.XPath

%access public export

data FreyjaError : Type where
  ExtractionError : XPathError -> FreyjaError
  GeneralError    : String -> FreyjaError
  TextConvError   : String -> FreyjaError
  MalformedDocError : String -> String -> FreyjaError

Show FreyjaError where
  show (ExtractionError msg) = unwords ["Extraction Error", show msg]
  show (TextConvError msg)   = unwords ["Text Conversion Error", show msg]
  show (GeneralError msg)    = msg
  show (MalformedDocError qstr msg) =
      unlines [ "Malformed Document Error."
              , " The query:"
              , unwords ["\t", show qstr]
              , "was expected to work,"
              , msg]

Extract : Type -> Type
Extract a = Either FreyjaError a

-- --------------------------------------------------------------------- [ EOF ]
