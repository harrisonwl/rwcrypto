{-# LANGUAGE DataKinds #-}
module Aes.Cipher256( encrypt256 ) where

import Prelude (($),foldl)
import ReWire
import ReWire.Bits ((^))
import ReWire.Vectors (index, generate)
import ReWire.Finite
import ReWire.FiniteComp as FC

import Aes.Basic (State, RoundKey , Key , KeySchedule , toByte4 , splitkey)
import Aes.AddRoundKey (addRoundKey)
import Aes.SubBytes (subbytes)
import Aes.ShiftRows (shiftrows)
import Aes.MixColumns (mixcolumns)
import Aes.KeyExp.Reference256 (keyexpansion)

import Aes.TestStates(states)

-- | AES parameters for AES-256
--          Key Length (Nk words) | Block Size (Nb words) | Number of Rounds (Nr)
-- -------------------------------------------------------------------------------
-- AES-256 |          8           |           4           |        14
-- -------------------------------------------------------------------------------

-- | Type for the expanded key schedule (AES-256)
-- type KeySchedule = Vec 60 (Vec 4 (W 8))  -- For AES-256: 15 round keys × 4 words × 4 bytes

-- | Extract a round key from the key schedule (AES-256)
-- Each round key is 4 words (16 bytes) = Vec 4 (Vec 4 (W 8))

extractRoundKey :: KeySchedule -> Finite 15 -> RoundKey
extractRoundKey ks f15 = fromList $ toByte4 (ks `index` i0)  -- correct order?
                                  : toByte4 (ks `index` i1) 
                                  : toByte4 (ks `index` i2)
                                  : toByte4 (ks `index` i3) : []
  where
    i0 , i1 , i2 , i3 :: Finite 60
    i0 = times4 f15
    i1 = times4 f15 FC.+ finite 1
    i2 = times4 f15 FC.+ finite 2
    i3 = times4 f15 FC.+ finite 3

    times4 :: Finite 15 -> Finite 60
    times4 f15 = (finite 4) FC.* (toFinite (toW4 f15))
      where
        toW4 :: Finite 15 -> W 4
        toW4 f15 = fromFinite f15


-- | The main Cipher function for AES-256 as defined in Figure 5 of NIST FIPS 197
-- Cipher(byte in[4*Nb], byte out[4*Nb], word w[Nb*(Nr+1)])
cipher :: State -> KeySchedule -> State
cipher state w = finalRound (rounds state w)
  where
    -- Initial round: AddRoundKey only
    initialRound :: State -> KeySchedule -> State
    initialRound s w = addRoundKey (extractRoundKey w 0) s
    
    -- Main rounds: SubBytes, ShiftRows, MixColumns, AddRoundKey
    rounds :: State -> KeySchedule -> State
    rounds s w = foldl roundFunction (initialRound s w) [1..13]  -- 13 rounds for AES-256
    
    roundFunction :: State -> Finite 15 -> State
    roundFunction s round = addRoundKey (extractRoundKey w round) 
                                       (mixcolumns (shiftrows (subbytes s)))
    
    -- Final round: SubBytes, ShiftRows, AddRoundKey (no MixColumns)
    finalRound :: State -> State
    finalRound s = addRoundKey (extractRoundKey w 14) 
                               (shiftrows (subbytes s))

-- 
-- This corresponds to Specification.cry's encrypt
-- 
encrypt256 :: Key -> State -> State
encrypt256 k inp = cipher inp (keyexpansion k)

-- | Alternative implementation using explicit round structure for AES-256
cipherExplicit :: State -> KeySchedule -> State
cipherExplicit state w = 
  let s0  = addRoundKey (extractRoundKey w  0) state
      s1  = addRoundKey (extractRoundKey w  1) (mixcolumns (shiftrows (subbytes s0)))
      s2  = addRoundKey (extractRoundKey w  2) (mixcolumns (shiftrows (subbytes s1)))
      s3  = addRoundKey (extractRoundKey w  3) (mixcolumns (shiftrows (subbytes s2)))
      s4  = addRoundKey (extractRoundKey w  4) (mixcolumns (shiftrows (subbytes s3)))
      s5  = addRoundKey (extractRoundKey w  5) (mixcolumns (shiftrows (subbytes s4)))
      s6  = addRoundKey (extractRoundKey w  6) (mixcolumns (shiftrows (subbytes s5)))
      s7  = addRoundKey (extractRoundKey w  7) (mixcolumns (shiftrows (subbytes s6)))
      s8  = addRoundKey (extractRoundKey w  8) (mixcolumns (shiftrows (subbytes s7)))
      s9  = addRoundKey (extractRoundKey w  9) (mixcolumns (shiftrows (subbytes s8)))
      s10 = addRoundKey (extractRoundKey w 10) (mixcolumns (shiftrows (subbytes s9)))
      s11 = addRoundKey (extractRoundKey w 11) (mixcolumns (shiftrows (subbytes s10)))
      s12 = addRoundKey (extractRoundKey w 12) (mixcolumns (shiftrows (subbytes s11)))
      s13 = addRoundKey (extractRoundKey w 13) (mixcolumns (shiftrows (subbytes s12)))
      s14 = addRoundKey (extractRoundKey w 14) (shiftrows (subbytes s13))
  in s14

{-
-- | Testing function
testCipher :: IO ()
testCipher = do
  putStrLn "Testing AES-256 Cipher function..."
  -- Add test cases here using the example from NIST FIPS 197
  putStrLn "Test completed"
-}

