{-# LANGUAGE DataKinds #-}
module AddRoundKey (addRoundKey) where

import Prelude (($))
import ReWire
import ReWire.Bits ((^))
import ReWire.Vectors (index , generate)

import AESBasic(State , RoundKey)

lkup :: Vec 4 (Vec 4 (W 8)) -> (Finite 4 , Finite 4) -> W 8
lkup s (i , j) = (s `index` i) `index` j

addRoundKey :: State -> RoundKey -> State
addRoundKey s rk = generate $ \ i ->
                   generate $ \ j ->
                      lkup s (i , j) ^ lkup rk (i , j)             
