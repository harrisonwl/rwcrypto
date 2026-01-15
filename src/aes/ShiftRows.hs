{-# LANGUAGE DataKinds #-}
module Aes.ShiftRows (shiftrows) where

import ReWire.FiniteComp as FC
import ReWire.Vectors(index , generate)

import Aes.Basic(State)
    
shiftrows :: State -> State
shiftrows v = generate $ \ i ->
              generate $ \ j ->
                           (v `index` i) `index` (j FC.- i)
