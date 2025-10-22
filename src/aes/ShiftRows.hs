{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module ShiftRows (shiftrows) where

import Prelude               as P
import ReWire
import ReWire.Bits           as B
import ReWire.Finite
import ReWire.FiniteComp     as FC
import ReWire.Vectors        as RWV

import AESBasic
-- type State  = Vec 4 (Vec 4 (W 8))
-- type Column = Vec 4 (W 8)
    
-- shiftrows :: KnownNat n => Vec n (Vec n a) -> Vec n (Vec n a)
shiftrows :: State -> State
shiftrows v = generate $ \ i ->
              generate $ \ j ->
                           (v `index` i) `index` (j FC.- i)
