{-# LANGUAGE DataKinds #-}
--module Aes.RW_AES256 where

import Prelude as P hiding ((-) , (*) , (==) , (<) , (^) , (/) , head , tail , round , (<>))
import ReWire
import ReWire.Bits as RB hiding ((<) , (*))
import ReWire.Vectors hiding (update)
import ReWire.Finite

import Aes.Basic(splitkey,Key,KeySchedule)
import Aes.KeyExp.Reference256(rnd , RF , ks0 , initKeySched)

-- | N.b., using the ReWire definitions for these transformers
-- | and not the "semantic" definitions from AES.ExtensionalSemantics
type ST s     = StateT s Identity
type Re i s o = ReacT i o (ST s)

--
-- I'm just going to go ahead and input Keys and Texts as whole blobs
-- rather than marshaling/unmarshaling.
-- 
data I   = Key (W 256)
         | Txt (W 128)
         | Round
         | Cont
      {- | M128 | M192 -} | M256 -- modes
        -- | Read Integer -- just for instrumentation

----
-- read and write key registers
----

-- putKS :: Finite 60 -> W 32 -> StateT RF Identity ()
putKS :: Finite 60 -> W 32 -> ST RF ()
putKS ix w = do
               (ks , c) <- get
               put ((ks != ix) w , c)

-- | applies Reference256.rnd
-- round :: StateT RF Identity ()
round :: ST RF ()
round = do
          (ks , c) <- get
          let (ks' , c') = rnd (ks , c)
          put (ks' , c')

-- loop :: I (W 32) -> StateT RF Identity ()
loop :: I -> Re I RF (Maybe (W 32)) ()
loop M256    = do
                  lift $ put (ks0 , finite 0)
                  i <- signal Nothing
                  loop i
loop (Key k) = do
                  lift (put (ks , finite 8))
                  i <- signal Nothing
                  loop i
  where
    ks :: KeySchedule
    ks = initKeySched k
loop Round   = do
                  lift round
                  i <- signal Nothing
                  loop i
loop Cont    = do
                  i <- signal Nothing
                  loop i
                  
start :: ReacT I (Maybe (W 32)) Identity ()
start = extrude (loop Cont) (ks0 , finite 0)

-- loop (KB                
{-          
hdl :: I (W 32) -> Re (I (W 32)) RF (Maybe (W 32)) b
hdl (KB w0)  = do
                   lift $ putKS (finite 0) w0
                   i1 <- signal Nothing
                   case i1 of
                     KB w1 -> do
                       lift $ putKS (finite 1) w1
                       i2 <- signal Nothing
                       case i2 of
                         KB w2 -> do
                           lift $ putKS (finite 2) w2
                           i3 <- signal Nothing
                           case i3 of
                             KB w3 -> do
                               lift $ putKS (finite 3) w3
                               i4 <- signal Nothing
                               case i4 of
                                 KB w4 -> do
                                   lift $ putKS (finite 4) w4
                                   i5 <- signal Nothing
                                   case i5 of
                                     KB w5 -> do
                                       lift $ putKS (finite 5) w5
                                       i6 <- signal Nothing
                                       case i6 of
                                         KB w6 -> do
                                           lift $ putKS (finite 6) w6
                                           i7 <- signal Nothing
                                           case i7 of
                                             KB w7 -> do
                                               lift $ putKS (finite 7) w7
                                               (ks , _) <- lift get
                                               lift $ put (ks , lit 8)
                                               i <- signal Nothing
                                               hdl i
                                             _      -> hdl i7
                                         _      -> hdl i6
                                     _      -> hdl i5
                                 _      -> hdl i4
                             _      -> hdl i3
                         _      -> hdl i2
                                   
                     _      -> hdl i1
hdl Round     = do
                  lift round 
                  i <- signal Nothing
                  hdl i
hdl Cont      = do
                  i <- signal Nothing
                  hdl i
-- hdl (Read ix) = do
--                   ({- _ ,-} ks , _) <- lift get
--                   i <- signal (Just (ks `index` (finite ix) ))
--                   hdl i
-}
