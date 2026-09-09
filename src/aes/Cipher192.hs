{-# LANGUAGE DataKinds #-}
module Aes.Cipher192(encrypt192) where

import Prelude (($),foldl)
import ReWire
import ReWire.Bits ((^))
import ReWire.Vectors (index, generate)
import ReWire.Finite
import ReWire.FiniteComp

import Aes.Basic (State, RoundKey , roundkey , Key , KeySchedule , toByte4 , splitkey , transpose , initState)
import Aes.Operations.AddRoundKey (addRoundKey)
import Aes.Operations.SubBytes (subbytes)
import Aes.Operations.ShiftRows (shiftrows)
import Aes.Operations.MixColumns (mixcolumns)
import Aes.KeyExp.KeyExpansion192 (keyexpand)


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
-- type KeySchedule = Vec 44 (Vec 4 (W 8))  -- For AES-128: 11 round keys × 4 words × 4 bytes

-- | The main Cipher function as defined in Figure 5 of NIST FIPS 197
-- Cipher(byte in[4*Nb], byte out[4*Nb], word w[Nb*(Nr+1)])
cipher :: State -> KeySchedule -> State
cipher state w = finalRound (rounds state w)
  where
    -- Initial round: AddRoundKey only
    initialRound :: State -> KeySchedule -> State
    initialRound s w = addRoundKey (roundkey w 0) s
    
    -- Main rounds: SubBytes, ShiftRows, MixColumns, AddRoundKey
    rounds :: State -> KeySchedule -> State
    rounds s w = foldl roundFunction (initialRound s w) [1..11]  -- 9 rounds for AES-128
    
    roundFunction :: State -> Finite 15 -> State
    roundFunction s round = addRoundKey (roundkey w round) 
                                        (mixcolumns (shiftrows (subbytes s)))
    
    -- Final round: SubBytes, ShiftRows, AddRoundKey (no MixColumns)
    finalRound :: State -> State
    finalRound s = addRoundKey (roundkey w 12) 
                               (shiftrows (subbytes s))

-- 
-- This corresponds to Specification.cry's encrypt
-- 
encrypt192 :: W 192 -> W 128 -> State
encrypt192 k inp = cipher (initState inp) (keyexpand k)
