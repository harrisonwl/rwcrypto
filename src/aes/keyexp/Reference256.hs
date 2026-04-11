{-# LANGUAGE DataKinds #-}
module Aes.KeyExp.Reference256 ( keyexpand
                               , extractRoundKey
                               , initKeySched
                               , ks0
                               , rnd
                               , RF )
   where

import Prelude as P hiding ((-) , (*) , (<) , (^) , (/) , head , tail , round)
import ReWire -- hiding (put , get , signal , lift)
import ReWire.Bits as RB hiding ((<) , (*))
import ReWire.Vectors hiding ((!=))
import ReWire.Finite
import ReWire.FiniteComp as FC

import Aes.Basic(Key , KeySchedule , RoundKey , (!=) , (@@@) , toW32 , splitkey , toByte4 , transpose)
import Aes.Operations.SubBytes(subword)
import Aes.Operations.RotWord(rotword)

-- |
-- | Standard semantics for Key Expansion
-- |

keyexpand :: Key -> KeySchedule
keyexpand k = fst . rnd . rnd . rnd . rnd . rnd .
                    rnd . rnd . rnd . rnd . rnd .
                    rnd . rnd . rnd $ (initKeySched k , finite 8)

-- | This accomplishes what initKS does, but in a vastly
-- | simpler manner.
initKeySched :: Key -> KeySchedule
initKeySched k = splitkey k ReWire.Vectors.++ ks52

-- | Extract a round key from the key schedule (AES-256)
-- Each round key is 4 words (16 bytes) = Vec 4 (Vec 4 (W 8))

extractRoundKey :: KeySchedule -> Finite 15 -> RoundKey
extractRoundKey ks f15 = transpose $
                            fromList [ toByte4 (ks `index` i0)  
                                     , toByte4 (ks `index` i1) 
                                     , toByte4 (ks `index` i2)
                                     , toByte4 (ks `index` i3) ]
  where

    times4 :: Finite 15 -> Finite 60
    times4 f15 = (finite 4) FC.* (toFinite (toW4 f15))
      where
        toW4 :: Finite 15 -> W 4
        toW4 f15 = fromFinite f15

    i0 , i1 , i2 , i3 :: Finite 60
    i0 = times4 f15
    i1 = times4 f15 FC.+ finite 1
    i2 = times4 f15 FC.+ finite 2
    i3 = times4 f15 FC.+ finite 3


--

type RF          = (KeySchedule, Finite 60)


-- |
-- | Purely functional version of round.
-- | N.b., it "unrolls" the loop 4 times each.
-- |

rnd :: (KeySchedule, Finite 60) -> (KeySchedule, Finite 60)
rnd = expand . expand . expand . expand
  where
    expand :: (KeySchedule, Finite 60) -> (KeySchedule, Finite 60)
    expand (ks , c) = (assign c (body c ks) ks , c FC.+ finite 1)

assign :: Finite 60 -> W 32 -> KeySchedule -> KeySchedule
assign c w32 ks = ks != c $ w32

-- | No @@@.
body :: Finite 60 -> KeySchedule -> W 32
body i w = let
                wi1 , wi8 , temp :: W 32 
                wi1   = w `index` (i FC.- finite 1) -- == temp in Fig. 11.
                wi8   = w `index` (i FC.- finite 8)
                iw , imod8 :: W 6
                iw    = fromFinite i
                imod8 = iw % lit 8
                temp  = if (imod8 RB.== lit 0)
                          then
                             subword(rotword wi1) ^ (toW32 (rcon (iw / (lit 8))))
                          else if (imod8 RB.== lit 4)
                                 then
                                   subword wi1
                                 else
                                   wi1
            in
                 wi8 ^ temp


rcon :: W 6 -> Vec 4 (W 8)
rcon _ = fromList [lit 9 , lit 9 , lit 9 , lit 9]

--fubar :: Vec 4 (W 8)
-- fubar = fromList [lit 9 , lit 9 , lit 9 , lit 9]

{- Working version
rcon :: W 6 -> Vec 4 (W 8)
rcon i = table5 @@@ (i RB.- lit 1)
  where
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
-}

{-
rcon' :: Finite 60 -> Vec 4 (W 8)
rcon' i = table5 @@@ (i RB.- lit 1)
  where
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
-}



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
--                     rnd . rnd . rnd $ (initKeySched k , lit 8)
