{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Aes.KeyExp.KeyExpansion256 ( keyexpand
                                  , initKeySched256
                                  , ks0
                                  , rnd
                                  , RF )
   where

import Prelude as P hiding ((-) , (*) , (<) , (^) , (/) , head , tail , round)
import ReWire -- hiding (put , get , signal , lift)
import ReWire.Bits as RB hiding ((<) , (*) , (==))
import ReWire.Vectors hiding ((!=))
import ReWire.Finite
import ReWire.FiniteComp as FC

import Aes.Basic(Key , KeySchedule , RoundKey
                 , roundkey , (!=) , toW32 , splitkey , toByte4 , transpose)
import Aes.Operations.SubBytes(subword)
import Aes.Operations.RotWord(rotword)

-- import ReWire.Interactive (dshow , hex , xshow)

type RF          = (KeySchedule, Finite 60)

-- | This accomplishes what initKS does, but in a vastly
-- | simpler manner.
initKeySched256 :: Key -> KeySchedule
initKeySched256 k = splitkey k ReWire.Vectors.++ ks52
   where
     -- | This is for the initialization of the keyschedule. Add the 8 words from the Key and
     -- | you've got an initialized keyschedule.
     ks52 :: Vec 52 (W 32)
     ks52 = fromList
              [ lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
              , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
              , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
              , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
              , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
              , lit 0 , lit 0 ]


-- |
-- | Standard semantics for Key Expansion
-- |
keyexpand :: Key -> KeySchedule
keyexpand k = fst . rnd . rnd . rnd . rnd . rnd .
                    rnd . rnd . rnd . rnd . rnd .
                    rnd . rnd . rnd $ (initKeySched256 k , finite 8)

-- |
-- | Purely functional version of round.
-- | N.b., it "unrolls" the loop 4 times each.
-- |

rnd :: (KeySchedule, Finite 60) -> (KeySchedule, Finite 60)
rnd = expand . expand . expand . expand
  where
    expand :: (KeySchedule, Finite 60) -> (KeySchedule, Finite 60)
    expand (ks , c) = ( assign c (body c ks) ks , c FC.+ finite 1)

assign :: Finite 60 -> W 32 -> KeySchedule -> KeySchedule
assign c w32 ks = ks != c $ w32

-- fubar :: Finite 60
-- fubar = FC.mod (finite 13) (finite 5)

-- | No @@@.
body :: Finite 60 -> KeySchedule -> W 32
body i w = let
                wi1 , wi8 , temp :: W 32 
                wi1   = w `index` (i FC.- finite 1) -- == temp in Fig. 11.
                wi8   = w `index` (i FC.- finite 8)
                im8 :: Finite 60
                im8 = i `FC.mod` (finite 8)
                temp  = if (im8 FC.== finite 0)
                          then
                             subword(rotword wi1) ^ (toW32 (rcon' (div8 i)))      
                          else if (im8 FC.== finite 4)
                                 then
                                   subword wi1
                                 else
                                   wi1
            in
                 wi8 ^ temp

div8 :: Finite 60 -> Finite 10
div8 i | idiv8 FC.== finite 0 = finite 0
       | idiv8 FC.== finite 1 = finite 1
       | idiv8 FC.== finite 2 = finite 2
       | idiv8 FC.== finite 3 = finite 3
       | idiv8 FC.== finite 4 = finite 4
       | idiv8 FC.== finite 5 = finite 5
       | idiv8 FC.== finite 6 = finite 6
       | idiv8 FC.== finite 7 = finite 7
       | otherwise            = finite 8 -- unreachable.
  where
    idiv8 :: Finite 60
    idiv8 = i `FC.div` (finite 8)

{- Older Working version uses @@@
rcon :: W 6 -> Vec 4 (W 8)
rcon i = table5 @@@ (i RB.- lit 1)
-}

{- Working version -}
rcon' :: Finite 10 -> Vec 4 (W 8)
rcon' i = table5 `index` (i FC.- (finite 1))

-- | N.b., in Table 5 on page 17 of nist.fips.197-upd, rcon is indexed from 1 to 10.
-- | Thanks NIST!
table5 :: Vec 10 (Vec 4 (W 8))
table5 = fromList [
                fromList [ lit 0x01, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x02, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x04, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x08, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x10, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x20, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x40, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x80, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x1b, lit 0x00, lit 0x00, lit 0x00],
                fromList [ lit 0x36, lit 0x00, lit 0x00, lit 0x00]
                ]

ks0 :: KeySchedule
ks0 = fromList
        [ lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 ]

-------------------------
-- Below is the putative junkyard
-------------------------
{-
initKS :: Key -> KeySchedule -> KeySchedule
initKS k = mv2ks (lit 0) k' .
           mv2ks (lit 1) k' .
           mv2ks (lit 2) k' .
           mv2ks (lit 3) k' .
           mv2ks (lit 4) k' . 
           mv2ks (lit 5) k' . 
           mv2ks (lit 6) k' . 
           mv2ks (lit 7) k' 
   where

     k' :: Vec 8 (W 32)
     k' = splitkey k

     mv2ks :: W 6 -> Vec 8 (W 32) -> Vec 60 (W 32) -> Vec 60 (W 32)
     mv2ks i k w = w != toFinite i $ k `index` (toFinite i)


keyexpansion :: Key -> KeySchedule
keyexpansion k = fst $ runST (expands k) (ks0 , lit 8)

expands :: Key -> ST (KeySchedule, W 6) KeySchedule
expands k =    do
                  put (initKS k ks0 , lit 8) -- initKS performs 8 "expand"s
                  round                      -- 13 "round"s perform 13*4 "expand"s
                  round                      -- total expands = 8 + 13*4 = 60
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  do
                    (ks,_) <- get
                    return ks

round :: ST RF ()
round = do
           rf <- get
           put (rnd rf)
-}

{-
keyexpand :: Integer -> Key -> ST RF (W 32)
keyexpand i k = do
                  put (initKS k ks0 , lit 8) -- initKS performs 8 "expand"s
                  round                      -- 13 "round"s perform 13*4 "expand"s
                  round                      -- total expands = 8 + 13*4 = 60
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  round
                  rdKS (finite i)

rdKS :: Finite 60 -> ST RF (W 32)
rdKS i = do
            (ks , _) <- get
            return (ks `index` i)

-}

-- -- | this uses @@@ and related hacks which RWC doesn't like.
-- rnd :: (KeySchedule, W 6) -> (KeySchedule, W 6)
-- rnd = expand . expand . expand . expand
--   where
--     expand :: (KeySchedule, W 6) -> (KeySchedule, W 6)
--     expand (ks , c) = (assign c (body c ks) ks , c RB.+ lit 1)

-- assign :: W 6 -> W 32 -> KeySchedule -> KeySchedule
-- assign c w32 ks = ks != (toFinite c) $ w32

-- body :: W 6 -> KeySchedule -> W 32
-- body i w = let
--                wi1 , wi8 , temp :: W 32 
--                imod8 :: W 6
--                wi1   = w @@@ (i RB.- lit 1) -- == temp in Fig. 11.
--                wi8   = w @@@ (i RB.- lit 8)
--                imod8 = i % (lit 8)
--                temp  = if (imod8 RB.== lit 0)
--                         then
--                            subword(rotword wi1) ^ (toW32 (rcon (i / (lit 8))))
--                         else if (imod8 RB.== lit 4)
--                                then
--                                  subword wi1
--                                else
--                                  wi1
--            in
--                wi8 ^ temp

-- keyexpand :: Key -> KeySchedule
-- keyexpand k = fst . rnd . rnd . rnd . rnd . rnd .
--                     rnd . rnd . rnd . rnd . rnd .
--                     rnd . rnd . rnd $ (initKeySched256 k , lit 8)
