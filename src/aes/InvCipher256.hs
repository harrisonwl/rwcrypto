{-# LANGUAGE DataKinds #-}
module Aes.InvCipher256(decrypt256) where

import Prelude (($),foldl,reverse)
import ReWire
import ReWire.Bits ((^))
import ReWire.Vectors (index, generate)
import ReWire.Finite
import ReWire.FiniteComp as FC

import Aes.Basic (State, RoundKey , Key , KeySchedule , toByte4 , splitkey , transpose)
import Aes.Operations.AddRoundKey (addRoundKey)
import Aes.Operations.InvSubBytes (invsubbytes)
import Aes.Operations.InvShiftRows (invshiftrows)
import Aes.Operations.InvMixColumns (invmixcolumns)
import Aes.KeyExp.Reference256 (keyexpand , roundkey)

import Aes.TestStates(states)

invround :: KeySchedule -> Finite 15 -> State -> State
invround w r s = invmixcolumns
                    (addRoundKey (roundkey w r)
                                 (invsubbytes (invshiftrows s)))

invfinalround :: KeySchedule -> State -> State
invfinalround w s = addRoundKey (roundkey w 0)
                                (invsubbytes (invshiftrows s))

-- 
-- This corresponds to Specification.cry's encrypt
-- 
decrypt256 :: Key -> State -> State
decrypt256 k inp = invcipher inp (keyexpand k)

-- decrypt : [KeySize] -> [BlockSize] -> [BlockSize]
-- decrypt k = invCipher (keyExpansion k)
-- decrypt k = _InvCipher (keyexpansion k)
-- _InvCipher :: KeySchedule -> State -> State
-- _InvCipher w ct = ct

invcipher :: State -> KeySchedule -> State
invcipher state w = invfinalround (rounds state w)

  where

    -- Initial round: AddRoundKey only
    initialRound :: State -> KeySchedule -> State
    initialRound s w = addRoundKey s (roundkey w 14)
    
    -- Main rounds: SubBytes, ShiftRows, MixColumns, AddRoundKey
    rounds :: State -> KeySchedule -> State
    rounds s w = foldl invround (initialRound s w) (Prelude.reverse [1..13])  -- 13 rounds for AES-256
    
    -- roundFunction :: State -> Finite 15 -> State
    -- roundFunction s round = addRoundKey (roundkey w round) 
    --                                     (mixcolumns (shiftrows (subbytes s)))

    invround :: State -> Finite 15 -> State
    invround s r = invmixcolumns
                         (addRoundKey (roundkey w r)
                                      (invsubbytes (invshiftrows s)))

    invfinalround :: State -> State
    invfinalround s = addRoundKey (roundkey w 0)
                                  (invsubbytes (invshiftrows s))

    -- -- Final round: SubBytes, ShiftRows, AddRoundKey (no MixColumns)
    -- finalRound :: State -> State
    -- finalRound s = addRoundKey (roundkey w 14) 
    --                            (shiftrows (subbytes s))

