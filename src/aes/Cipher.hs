{-# LANGUAGE DataKinds #-}
module AES.Cipher where

import Prelude (($))
import ReWire
import ReWire.Bits ((^))
import ReWire.Vectors (index , generate)
import ReWire.Finite
import ReWire.FiniteComp

import AES.Basic(State)
import AES.AddRoundKey(addRoundKey)
import AES.SubBytes(subbytes)
import AES.ShiftRows(shiftrows)
import AES.MixColumns(mixColumns)


--          Key Length (Nk words) | Block Size (Nb words) | Number of Rounds (Nr)
-- -------------------------------------------------------------------------------
-- AES-128 |          4           |           4           |        10
-- -------------------------------------------------------------------------------
-- AES-192 |          6           |           4           |        12
-- -------------------------------------------------------------------------------
-- AES-256 |          8           |           4           |        14
--

lkup :: Vec 4 (Vec 4 (W 8)) -> (Finite 4 , Finite 4) -> W 8
lkup s (i , j) = (s `index` i) `index` j

type WA = Vec 60 (W 32)


-- proj :: Finite 15 -> WA -> Vec 4 (W 32)
-- proj i w = generate $ \ j -> w `index` ((i * finite 4) + j)

-- proj :: Finite 60 -> WA -> Vec 4 (W 32)
proj i w = generate $ \ j -> w `index` ((i * 4) + j)

foo :: Finite 15 -> Finite 60
foo i = i 

body :: Monad m => p -> StateT State m ()
body round = do
  modify subbytes
  modify shiftrows
  modify mixColumns
  -- addRoundKey
  
-- addRoundKey :: State -> RoundKey -> State
-- type RoundKey = Vec 4 (Vec 4 (W 8))
--    =~ Vec 4 (W 32)
-- subbytes :: State -> State
-- shiftrows :: State -> State
-- mixColumns :: State -> State

