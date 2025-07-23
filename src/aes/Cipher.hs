{-# LANGUAGE DataKinds #-}
module Cipher where

import Prelude (($))
import ReWire
import ReWire.Bits ((^))
import ReWire.Vectors (index , generate)

import AESBasic(State)
import AddRoundKey(addRoundKey)
import SubBytes(subbytes)
import ShiftRows(shiftrows)
import MixColumns(mixColumns)


--          Key Length (Nk words) | Block Size (Nb words) | Number of Rounds (Nr)
-- -------------------------------------------------------------------------------
-- AES-128 |          4           |           4           |        10
-- -------------------------------------------------------------------------------
-- AES-192 |          6           |           4           |        12
-- -------------------------------------------------------------------------------
-- AES-256 |          8           |           4           |        14
--

type State    = Vec 4 (Vec 4 (W 8))
lkup :: Vec 4 (Vec 4 (W 8)) -> (Finite 4 , Finite 4) -> W 8
lkup s (i , j) = (s `index` i) `index` j

type WA = Vec 60 (W 32)



body :: Monad m => p -> StateT AESBasic.State m ()
body round = do
  modify subbytes
  modify shiftrows
  modify mixColumns
  -- addRoundKey
  
-- addRoundKey :: State -> RoundKey -> State
-- subbytes :: State -> State
-- shiftrows :: State -> State
-- mixColumns :: State -> State

