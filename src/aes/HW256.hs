{-# LANGUAGE DataKinds #-}
-- module Aes.HW256 where

import ReWire
import ReWire.Bits as RB 
import ReWire.Finite 
import ReWire.FiniteComp as FC

import Aes.Basic (State , RoundKey , KeySchedule , Key , initState , finalState , roundkey)
import Aes.Operations.AddRoundKey (addRoundKey)
import Aes.Operations.SubBytes (subbytes)
import Aes.Operations.ShiftRows (shiftrows)
import Aes.Operations.MixColumns (mixcolumns)
import Aes.KeyExp.KeyExpansion256 (keyexpand , ks0)

type RegF     = (KeySchedule, Finite 15 , State)
type ST s     = StateT s Identity
type Re i s o = ReacT i o (ST s)

---
--- Operations
---

incRC :: ST RegF ()
incRC = do
           (ks,i,s) <- get
           put (ks, i FC.+ (finite 1) , s)
           
roundM :: ST RegF ()
roundM = do
            (_,i,_) <- get
            if i FC.== finite 0 then
               do
                  rk <- roundkeyM (finite 0)
                  addRoundKeyM rk
                  incRC
            else if i FC.< finite 14 then
               do
                  rk <- roundkeyM i
                  subbytesM
                  shiftrowsM 
                  mixcolumnsM
                  addRoundKeyM rk
                  incRC
            else
               do
                  rk <- roundkeyM (finite 14)
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
                    return (roundkey ks ix)

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
                 return (finalState s)

---
--- Making HW out of this.
---

data I   = Key (W 256)
         | Txt (W 128)
         | Round
         | Answer
         | Cont

loop :: I -> Re I RegF (Maybe (W 128)) ()
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

start :: ReacT I (Maybe (W 128)) Identity ()
start = extrude (loop Cont) (ks0 , finite 0 , initState (lit 0))
