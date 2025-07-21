{-# LANGUAGE DataKinds #-}
module RotWord where

import Prelude hiding (head,tail)
import ReWire
import ReWire.Bits
import ReWire.Vectors

rotword :: Vec 4 (W 8) -> Vec 4 (W 8)
rotword v4 = tail v4 `snoc` head v4

v4 :: Vec 4 (W 8)
v4 = fromList [lit 0 , lit 1 , lit 2 , lit 3]
