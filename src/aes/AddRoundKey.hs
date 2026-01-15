{-# LANGUAGE DataKinds #-}
module Aes.AddRoundKey (addRoundKey) where

import Prelude (($))
import ReWire.Bits ((^))
import ReWire.Vectors (generate)

import Aes.Basic(State , RoundKey , lkup)

addRoundKey :: RoundKey -> State -> State
addRoundKey rk s = generate $ \ i ->
                   generate $ \ j ->
                      lkup s (i , j) ^ lkup rk (i , j)             
