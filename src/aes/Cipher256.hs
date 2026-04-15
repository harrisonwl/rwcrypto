{-# LANGUAGE DataKinds #-}
module Aes.Cipher256( encrypt256 ) where

import Prelude (($),foldl)
import ReWire
import ReWire.Bits ((^))
import ReWire.Vectors (index, generate)
import ReWire.Finite
import ReWire.FiniteComp as FC

import Aes.Basic (State, RoundKey , Key , KeySchedule , toByte4 , splitkey , transpose , initState)
import Aes.Operations.AddRoundKey (addRoundKey)
import Aes.Operations.SubBytes (subbytes)
import Aes.Operations.ShiftRows (shiftrows)
import Aes.Operations.MixColumns (mixcolumns)
import Aes.KeyExp.Reference256 (keyexpand , roundkey)

import Aes.TestStates(states)

-- | AES parameters for AES-256
--          Key Length (Nk words) | Block Size (Nb words) | Number of Rounds (Nr)
-- -------------------------------------------------------------------------------
-- AES-256 |          8           |           4           |        14
-- -------------------------------------------------------------------------------

-- | Type for the expanded key schedule (AES-256)
-- type KeySchedule = Vec 60 (Vec 4 (W 8))
-- For AES-256: 15 round keys × 4 words × 4 bytes


-- | The main Cipher function for AES-256 as defined in Figure 5 of NIST FIPS 197
-- Cipher(byte in[4*Nb], byte out[4*Nb], word w[Nb*(Nr+1)])
cipher :: State -> KeySchedule -> State
cipher state w = finalRound (rounds state w)
  where
    -- Initial round: AddRoundKey only
    initialRound :: State -> KeySchedule -> State
    initialRound s w = addRoundKey (roundkey w 0) s
    
    -- Main rounds: SubBytes, ShiftRows, MixColumns, AddRoundKey
    rounds :: State -> KeySchedule -> State
    rounds s w = foldl roundFunction (initialRound s w) [1..13]  -- 13 rounds for AES-256
    
    roundFunction :: State -> Finite 15 -> State
    roundFunction s round = addRoundKey (roundkey w round) 
                                        (mixcolumns (shiftrows (subbytes s)))
    
    -- Final round: SubBytes, ShiftRows, AddRoundKey (no MixColumns)
    finalRound :: State -> State
    finalRound s = addRoundKey (roundkey w 14) 
                               (shiftrows (subbytes s))

-- 
-- This corresponds to Specification.cry's encrypt
-- 
encrypt256 :: Key -> W 128 -> State
encrypt256 k inp = cipher (initState inp) (keyexpand k)

{-
-- | Alternative implementation using explicit round structure for AES-256
cipherExplicit :: State -> KeySchedule -> State
cipherExplicit state w = 
  let s0  = addRoundKey (roundkey w  0) state
      s1  = addRoundKey (roundkey w  1) (mixcolumns (shiftrows (subbytes s0)))
      s2  = addRoundKey (roundkey w  2) (mixcolumns (shiftrows (subbytes s1)))
      s3  = addRoundKey (roundkey w  3) (mixcolumns (shiftrows (subbytes s2)))
      s4  = addRoundKey (roundkey w  4) (mixcolumns (shiftrows (subbytes s3)))
      s5  = addRoundKey (roundkey w  5) (mixcolumns (shiftrows (subbytes s4)))
      s6  = addRoundKey (roundkey w  6) (mixcolumns (shiftrows (subbytes s5)))
      s7  = addRoundKey (roundkey w  7) (mixcolumns (shiftrows (subbytes s6)))
      s8  = addRoundKey (roundkey w  8) (mixcolumns (shiftrows (subbytes s7)))
      s9  = addRoundKey (roundkey w  9) (mixcolumns (shiftrows (subbytes s8)))
      s10 = addRoundKey (roundkey w 10) (mixcolumns (shiftrows (subbytes s9)))
      s11 = addRoundKey (roundkey w 11) (mixcolumns (shiftrows (subbytes s10)))
      s12 = addRoundKey (roundkey w 12) (mixcolumns (shiftrows (subbytes s11)))
      s13 = addRoundKey (roundkey w 13) (mixcolumns (shiftrows (subbytes s12)))
      s14 = addRoundKey (roundkey w 14) (shiftrows (subbytes s13))
  in s14
-}

{-
-- | Testing function
testCipher :: IO ()
testCipher = do
  putStrLn "Testing AES-256 Cipher function..."
  -- Add test cases here using the example from NIST FIPS 197
  putStrLn "Test completed"
-}

