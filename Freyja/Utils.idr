-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Freyja.Utils

export
mapEither : (a -> Either e b) -> List a -> Either e (List b)
mapEither f Nil     = pure Nil
mapEither f (x::xs) = do
  x'  <- f x
  xs' <- mapEither f xs
  pure (x' :: xs')

-- --------------------------------------------------------------------- [ EOF ]
