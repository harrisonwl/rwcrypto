{-# LANGUAGE DataKinds #-}
module Aes.RotWord where

import Prelude hiding (head,tail)
import ReWire
import ReWire.Bits
import ReWire.Vectors

import Aes.Basic (toW32 , toByte4)

rotword :: W 32 -> W 32
rotword w32 = toW32 (rot4 (toByte4 w32))
  where
    rot4 :: Vec 4 (W 8) -> Vec 4 (W 8)
    rot4 v4 = tail v4 `snoc` head v4

v4 :: Vec 4 (W 8)
v4 = fromList [lit 0 , lit 1 , lit 2 , lit 3]
