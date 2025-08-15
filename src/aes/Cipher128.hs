{-# LANGUAGE DataKinds #-}
module AES.Cipher where

import Prelude (($),foldl)
import ReWire
import ReWire.Bits ((^))
import ReWire.Vectors (index, generate)
import ReWire.Finite
import ReWire.FiniteComp

import AES.Basic (State, RoundKey)
import AES.AddRoundKey (addRoundKey)
import AES.SubBytes (subbytes)
import AES.ShiftRows (shiftrows)
import AES.MixColumns (mixColumns)

-- | AES parameters
-- Key Length (Nk words) | Block Size (Nb words) | Number of Rounds (Nr)
-- -------------------------------------------------------------------------------
-- AES-128 |          4           |           4           |        10
-- -------------------------------------------------------------------------------
-- AES-192 |          6           |           4           |        12
-- -------------------------------------------------------------------------------
-- AES-256 |          8           |           4           |        14
-- -------------------------------------------------------------------------------

-- | Type for the expanded key schedule
type KeySchedule = Vec 44 (Vec 4 (W 8))  -- For AES-128: 11 round keys × 4 words × 4 bytes

-- | Extract a round key from the key schedule
-- Each round key is 4 words (16 bytes) = Vec 4 (Vec 4 (W 8))
extractRoundKey :: KeySchedule -> Finite 11 -> RoundKey
extractRoundKey w round = generate $ \i -> generate $ \j -> 
  w `index` (roundIndex round i) `index` j
  where
    -- Convert round number and word index to key schedule index
    roundIndex :: Finite 11 -> Finite 4 -> Finite 44
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

-- | The main Cipher function as defined in Figure 5 of NIST FIPS 197
-- Cipher(byte in[4*Nb], byte out[4*Nb], word w[Nb*(Nr+1)])
cipher :: State -> KeySchedule -> State
cipher state w = finalRound (rounds state w)
  where
    -- Initial round: AddRoundKey only
    initialRound :: State -> KeySchedule -> State
    initialRound s w = addRoundKey (extractRoundKey w 0) s
    
    -- Main rounds: SubBytes, ShiftRows, MixColumns, AddRoundKey
    rounds :: State -> KeySchedule -> State
    rounds s w = foldl roundFunction (initialRound s w) [1..9]  -- 9 rounds for AES-128
    
    roundFunction :: State -> Finite 11 -> State
    roundFunction s round = addRoundKey (extractRoundKey w round) 
                                       (mixColumns (shiftrows (subbytes s)))
    
    -- Final round: SubBytes, ShiftRows, AddRoundKey (no MixColumns)
    finalRound :: State -> State
    finalRound s = addRoundKey (extractRoundKey w 10) 
                               (shiftrows (subbytes s))

-- | Alternative implementation using explicit round structure
cipherExplicit :: State -> KeySchedule -> State
cipherExplicit state w = 
  let s0 = addRoundKey (extractRoundKey w 0) state
      s1 = addRoundKey (extractRoundKey w 1) (mixColumns (shiftrows (subbytes s0)))
      s2 = addRoundKey (extractRoundKey w 2) (mixColumns (shiftrows (subbytes s1)))
      s3 = addRoundKey (extractRoundKey w 3) (mixColumns (shiftrows (subbytes s2)))
      s4 = addRoundKey (extractRoundKey w 4) (mixColumns (shiftrows (subbytes s3)))
      s5 = addRoundKey (extractRoundKey w 5) (mixColumns (shiftrows (subbytes s4)))
      s6 = addRoundKey (extractRoundKey w 6) (mixColumns (shiftrows (subbytes s5)))
      s7 = addRoundKey (extractRoundKey w 7) (mixColumns (shiftrows (subbytes s6)))
      s8 = addRoundKey (extractRoundKey w 8) (mixColumns (shiftrows (subbytes s7)))
      s9 = addRoundKey (extractRoundKey w 9) (mixColumns (shiftrows (subbytes s8)))
      s10 = addRoundKey (extractRoundKey w 10) (shiftrows (subbytes s9))
  in s10

{-
-- | Testing function
testCipher :: IO ()
testCipher = do
  putStrLn "Testing AES Cipher function..."
  -- Add test cases here using the example from NIST FIPS 197
  putStrLn "Test completed"
-}
