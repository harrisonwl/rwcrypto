{-# LANGUAGE DataKinds #-}
module Aes.KeyExp.Hardware256 where

import Prelude as P hiding ((-) , (*) , (==) , (<) , (^) , (/) , head , tail , round , (<>))
import ReWire hiding (put , get , signal , lift)
import ReWire.Bits as RB hiding ((<) , (*))
import ReWire.Vectors hiding (update)
import ReWire.Finite

import Aes.ExtensionalSemantics
import Aes.KeyExp.Reference256(rnd , RF)

------
-- Hardware Semantics for key size 256. By which I mean, the reference semantics
-- reformulated into a reactive resumption style. In Reference256, the main function
-- is "keyexpand":
--      * typed in the state monad
--      * corresponds to nist.fips.197, Fig.11
--      * loops are unrolled
-- In the Hardware semantics, the main functions are
--     1. handler that responds input signals (i.e., I Key), and
--          hdl :: I Key -> Re (I Key) (KeySchedule, W 6) (Maybe (W 32)) ()
--     2. an sequential semantics in the form of NFM23 semantics
--        expressed in Haskell
--          exec :: Re (I i) s o () ->                        
--                  (I i, s, o) -> [I i] -> Stream (I i, s, o)
------

--
-- Given that the key is either 128, 192, or 256 bits, I'll assume that the
-- machine will read in 64 bits of key at a time.
-- 
data I a = KB a  
         | Round
         | Cont
         | Read Integer -- just for instrumentation
        -- | M128 | M192 | M256 -- modes

----
-- read and write key registers
----

putKS :: Finite 60 -> W 32 -> ST RF ()
putKS ix w = do
               (ks , c) <- get
               put ((ks != ix) w , c)

-- | applies Reference256.rnd
round :: ST RF ()
round = do
          (ks , c) <- get
          let (ks' , c') = rnd (ks , c)
          put (ks' , c')
          
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
hdl (Read ix) = do
                  ({- _ ,-} ks , _) <- lift get
                  i <- signal (Just (ks `index` (finite ix) ))
                  hdl i
