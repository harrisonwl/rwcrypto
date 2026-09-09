{-# LANGUAGE DataKinds #-}
-- module Aes.RW_AES256 where

import Prelude as P hiding ((-) , (*) , (<) , (^) , (/) , head , tail , round , (<>))
import ReWire
import ReWire.Bits as RB hiding ((<) , (*))
import ReWire.Vectors hiding (update)
import ReWire.Finite
import Aes.Basic(splitkey , Key , KeySchedule , State , RoundKey , roundkey , initState , finalState)
import Aes.KeyExp.KeyExpansion256(rnd , RF , ks0 , initKeySched256)
import Aes.Operations.AddRoundKey (addRoundKey)
import Aes.Operations.SubBytes (subbytes)
import Aes.Operations.ShiftRows (shiftrows)
import Aes.Operations.MixColumns (mixcolumns)

-- | N.b., using the ReWire definitions for these transformers
-- | and not the "semantic" definitions from AES.ExtensionalSemantics
type ST s     = StateT s Identity
type Re i s o = ReacT i o (ST s)
type RegF     = (KeySchedule, Finite 60 , State)

initStateM :: W 128 -> ST RegF ()
initStateM inp = do
                   (ks , c , _) <- get
                   put (ks , c , initState inp)

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

--
-- I'm just going to go ahead and input Keys and Texts as whole blobs
-- rather than marshaling/unmarshaling.
-- 
data I   = Key (W 256)
         | Txt (W 128)
         | KERound
         | Round0
         | Roundi (Finite 15)
         | Roundf
         | Cont
      {- | M128 | M192 -} | M256 -- modes
        -- | Read Integer -- just for instrumentation

----
-- read and write key registers
----

putKS :: Finite 60 -> W 32 -> ST RegF ()
putKS ix w = do
               (ks , c , s) <- get
               put ((ks != ix) w , c , s)

-- | applies Reference256.rnd
round :: ST RegF ()
round = do
          (ks , c , s) <- get
          let (ks' , c') = rnd (ks , c)
          put (ks' , c', s)

loop :: I -> Re I RegF (Maybe (W 128)) ()
loop M256       = do
                     lift $ put (ks0 , finite 0 , initState (lit 0))
                     i <- signal Nothing
                     loop i
loop (Key k)    = do
                     lift (put (ks , finite 8, initState (lit 0)))
                     i <- signal Nothing
                     loop i
  where
    ks :: KeySchedule
    ks = initKeySched256 k
loop (Txt inp)  = do
                     lift (initStateM inp)
                     i <- signal Nothing
                     loop i
loop KERound    = do
                     lift round
                     i <- signal Nothing
                     loop i
loop Round0     = do
                     lift $ do
                               rk <- roundkeyM (finite 0)
                               addRoundKeyM rk                  
                     i <- signal Nothing
                     loop i
loop (Roundi j) = do
                     lift $ do
                               rk <- roundkeyM j
                               subbytesM
                               shiftrowsM
                               mixcolumnsM
                               addRoundKeyM rk                  
                     i <- signal Nothing
                     loop i
loop Roundf     = do
                     ct <- lift $ do
                                     rk <- roundkeyM (finite 14)
                                     subbytesM
                                     shiftrowsM
                                     addRoundKeyM rk
                                     answer
                     i <- signal (Just ct)
                     loop i
loop Cont    = do
                  i <- signal Nothing
                  loop i
                  
start :: ReacT I (Maybe (W 128)) Identity ()
start = extrude (loop Cont) (ks0 , finite 0 , initState (lit 0))
