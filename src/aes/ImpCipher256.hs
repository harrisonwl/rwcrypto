{-# LANGUAGE DataKinds #-}
module Aes.ImpCipher256( encrypt256M , RegF , encrypt256hw ) where

import Prelude (($) , foldl , fst , Maybe(..) , Show(..) , (++))
import ReWire hiding (put , get , signal , lift , extrude)
import ReWire.Bits ((^) , lit)
import ReWire.Vectors (index, generate)
import ReWire.Finite
import ReWire.FiniteComp as FC

import Aes.Basic (State, RoundKey , roundkey , Key , KeySchedule , toByte4 , splitkey , transpose , initState , finalState)
import Aes.Operations.AddRoundKey (addRoundKey)
import Aes.Operations.SubBytes (subbytes)
import Aes.Operations.ShiftRows (shiftrows)
import Aes.Operations.MixColumns (mixcolumns)
import Aes.KeyExp.KeyExpansion256 (keyexpand , ks0)

import Aes.ExtensionalSemantics

import ReWire.Interactive (xshow)

-- | AES parameters for AES-256
--          Key Length (Nk words) | Block Size (Nb words) | Number of Rounds (Nr)
-- -------------------------------------------------------------------------------
-- AES-256 |          8           |           4           |        14
-- -------------------------------------------------------------------------------

-- | Type for the expanded key schedule (AES-256)
-- type KeySchedule = Vec 60 (Vec 4 (W 8))
-- For AES-256: 15 round keys × 4 words × 4 bytes

-- 
-- This corresponds to Specification.cry's encrypt
-- 
encrypt256 :: W 256 -> W 128 -> State
encrypt256 k inp = cipher (initState inp) (keyexpand k)

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

type RegF     = (KeySchedule, Finite 15 , State)

-- -- | works, but is kruft
-- encrypt256M' :: Key -> W 128 -> ST RegF (W 128)
-- encrypt256M' k pt = do
--                       keyexpandM k
--                       initStateM pt
--                       initialRoundM
--                       roundFunctionM 1
--                       roundFunctionM 2
--                       roundFunctionM 3
--                       roundFunctionM 4
--                       roundFunctionM 5
--                       roundFunctionM 6
--                       roundFunctionM 7
--                       roundFunctionM 8
--                       roundFunctionM 9
--                       roundFunctionM 10
--                       roundFunctionM 11
--                       roundFunctionM 12
--                       roundFunctionM 13
--                       finalRoundM
--                       answer

encrypt256M :: Key -> W 128 -> ST RegF (W 128)
encrypt256M k pt = do
                      keyexpandM k
                      initStateM pt
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      roundM
                      answer

incRC :: ST RegF ()
incRC = do
           (ks,i,s) <- get
           put (ks, i+1 , s)
           
roundM :: ST RegF ()
roundM = do
            (_,i,_) <- get
            if i == finite 0 then
               do
                  rk <- roundkeyM (finite 0)
                  addRoundKeyM rk
                  incRC
            else if i < finite 14 then
               do
                  rk <- roundkeyM i
                  subbytesM
                  shiftrowsM 
                  mixcolumnsM
                  addRoundKeyM rk
                  incRC
            else
               do
                  rk <- roundkeyM 14
                  subbytesM
                  shiftrowsM 
                  addRoundKeyM rk


initialRoundM :: ST RegF ()
initialRoundM        = do
                          rk <- roundkeyM 0
                          addRoundKeyM rk

roundFunctionM :: Finite 15 -> ST RegF ()
roundFunctionM round = do
                          rk <- roundkeyM round
                          subbytesM
                          shiftrowsM 
                          mixcolumnsM
                          addRoundKeyM rk

finalRoundM :: ST RegF ()
finalRoundM          = do
                          rk <- roundkeyM 14
                          subbytesM
                          shiftrowsM 
                          addRoundKeyM rk


keyexpandM :: Key -> ST RegF ()
keyexpandM k = do
                  (_ , c , s) <- get
                  put (keyexpand k , c , s)

initStateM :: W 128 -> ST RegF ()
initStateM inp = do
                   (ks , _ , _) <- get
                   put (ks , finite 0 , initState inp)

addRoundKeyM :: RoundKey -> ST RegF ()
addRoundKeyM rk = do
                    (ks , c , s) <- get
                    put (ks , c , addRoundKey rk s)

roundkeyM :: Finite 15 -> ST RegF RoundKey
roundkeyM ix    = do
                    (ks , _ , _) <- get
                    returnS (roundkey ks ix)

subbytesM :: ST RegF ()
subbytesM   = do
                 (ks , c , s) <- get
                 put (ks , c , subbytes s)

shiftrowsM :: ST RegF ()
shiftrowsM  = do
                 (ks , c , s) <- get
                 put (ks , c , shiftrows s)

mixcolumnsM :: ST RegF ()
mixcolumnsM = do
                 (ks , c , s) <- get
                 put (ks , c , mixcolumns s)

answer :: ST RegF (W 128)
answer      = do
                 (_ , _ , s) <- get
                 returnS (finalState s)

---
--- Making HW out of this.
---

data I   = Key (W 256)
         | Txt (W 128)
         | Round
         | Answer
         | Cont

instance Show I where
  show (Key w) = "Key " ++ xshow w
  show (Txt t) = "Txt " ++ xshow t
  show Round   = "Round"
  show Answer  = "Answer"
  show Cont    = "Cont"
  

loop (Key k)   = do
                    lift (keyexpandM k)
                    i <- signal Nothing
                    loop i
loop (Txt inp) = do
                    lift (initStateM inp)
                    i <- signal Nothing
                    loop i
loop Round     = do
                    lift roundM
                    i <- signal Nothing
                    loop i
loop Cont      = do
                    i <- signal Nothing
                    loop i
loop Answer    = do
                    ct <- lift answer
                    i <- signal (Just ct)
                    loop i

-- start = re_inf (loop Cont) (Cont , s0 , Nothing)
--   where
--     s0 = (ks0 , finite 0 , initState (lit 0))

-- | runs the loop on a well-formed call
encrypt256hw :: W 256 -> W 128 -> WriterPlus (I, RegF, Maybe (W 128)) ()
encrypt256hw k pt = stepper (loop Cont) (Cont , s0 , Nothing) (aes256 k pt)
    where
       s0 = (ks0 , finite 0 , initState (lit 0))

-- | calling convention for HW.
aes256 :: W 256 -> W 128 -> [I]
aes256 k pt = Key k
            : Txt pt
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Round
            : Answer
            : Cont : []
                         
