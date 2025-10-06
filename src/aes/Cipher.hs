{-# LANGUAGE DataKinds #-}
module Cipher where

import Prelude (($))
import ReWire
import ReWire.Bits -- ((^),lit)
import ReWire.Vectors -- (index , generate)
import ReWire.Finite
import ReWire.FiniteComp

import AESBasic(State,RoundKey)
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

-- |
-- | For AES-256, Nb * (Nr + 1) = 60
-- |
type KeySchedule = Vec 60 (Vec 4 (W 8))

------------------
-- not complete --
------------------

body :: Monad m =>
        KeySchedule -> Finite 15 -> StateT State m ()
body w round = do
  modify subbytes
  modify shiftrows
  modify mixColumns
  modify (addRoundKey (extractRoundKey w round))

extractRoundKey :: KeySchedule -> Finite 15 -> RoundKey
extractRoundKey w round = generate $ \ i -> w `index` (roundIndex round i) 
  where
    -- Admittedly, super-hacky. Need some conversion operators
    -- to inject, e.g., Finite 15 -> Finite 60.
    -- Convert round number and word index to key schedule index
    roundIndex :: Finite 15 -> Finite 4 -> Finite 60
    roundIndex r i = case (r, i) of
      (0, 0) -> 0
      (0, 1) -> 1
      (0, 2) -> 2
      (0, 3) -> 3
      (1, 0) -> 4
      (1, 1) -> 5
      (1, 2) -> 6
      (1, 3) -> 7
      (2, 0) -> 8
      (2, 1) -> 9
      (2, 2) -> 10
      (2, 3) -> 11
      (3, 0) -> 12
      (3, 1) -> 13
      (3, 2) -> 14
      (3, 3) -> 15
      (4, 0) -> 16
      (4, 1) -> 17
      (4, 2) -> 18
      (4, 3) -> 19
      (5, 0) -> 20
      (5, 1) -> 21
      (5, 2) -> 22
      (5, 3) -> 23
      (6, 0) -> 24
      (6, 1) -> 25
      (6, 2) -> 26
      (6, 3) -> 27
      (7, 0) -> 28
      (7, 1) -> 29
      (7, 2) -> 30
      (7, 3) -> 31
      (8, 0) -> 32
      (8, 1) -> 33
      (8, 2) -> 34
      (8, 3) -> 35
      (9, 0) -> 36
      (9, 1) -> 37
      (9, 2) -> 38
      (9, 3) -> 39
      (10, 0) -> 40
      (10, 1) -> 41
      (10, 2) -> 42
      (10, 3) -> 43
      (11, 0) -> 44
      (11, 1) -> 45
      (11, 2) -> 46
      (11, 3) -> 47
      (12, 0) -> 48
      (12, 1) -> 49
      (12, 2) -> 50
      (12, 3) -> 51
      (13, 0) -> 52
      (13, 1) -> 53
      (13, 2) -> 54
      (13, 3) -> 55
      (14, 0) -> 56
      (14, 1) -> 57
      (14, 2) -> 58
      (14, 3) -> 59
