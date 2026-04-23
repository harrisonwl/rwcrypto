{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Aes.KeyExp.KeyExpansion128
                                  ( keyexpand
                                  , roundkey
                                  , initKeySched128
                                  , splitkey128
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

import Aes.Basic(Key , KeySchedule , RoundKey , (!=) , {- (@@@) , -} toW32 , splitkey , toByte4 , transpose)
import Aes.Operations.SubBytes(subword)
import Aes.Operations.RotWord(rotword)

-- import ReWire.Interactive (dshow , hex , xshow)


-- | AES parameters for AES-128
--          Key Length (Nk words) | Block Size (Nb words) | Number of Rounds (Nr)
-- -------------------------------------------------------------------------------
-- AES-256 |          4           |           4           |        10
-- -------------------------------------------------------------------------------

splitkey128 :: W 128 -> Vec 4 (W 32)
splitkey128 key = fromList [s0 , s1 , s2 , s3]
  where
    s0 , s1 , s2 , s3 :: W 32
    s0 = slice (Proxy :: Proxy 0)   key
    s1 = slice (Proxy :: Proxy 32)  key
    s2 = slice (Proxy :: Proxy 64)  key
    s3 = slice (Proxy :: Proxy 96)  key


-- | For AES-128, Nb * (Nr + 1) = 4 * (10 + 1) = 44, hence the key schedule has 60 elements:
-- |
-- | Note that we haven't changed the size of KeySchedule.
-- | 
type RF          = (KeySchedule, Finite 44)

initKeySched128 :: W 128 -> KeySchedule
initKeySched128 k = splitkey128 k ReWire.Vectors.++ ks56
  where
    -- | This is for the initialization of the keyschedule. 
    ks56 :: Vec 56 (W 32)
    ks56 = fromList
             [ lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 ]

-- | Extract a round key from the key schedule (AES-256)
-- | Each round key is 4 words (16 bytes) = Vec 4 (Vec 4 (W 8))
roundkey :: KeySchedule -> Finite 15 -> RoundKey
roundkey ks f15 = transpose $
                            fromList [ toByte4 (ks `index` i0)  
                                     , toByte4 (ks `index` i1) 
                                     , toByte4 (ks `index` i2)
                                     , toByte4 (ks `index` i3) ]
  where

    times4 :: Finite 15 -> Finite 60
    times4 f15 = (finite 4) FC.* (xfinite f15)

    i0 , i1 , i2 , i3 :: Finite 60
    i0 = times4 f15
    i1 = times4 f15 FC.+ finite 1
    i2 = times4 f15 FC.+ finite 2
    i3 = times4 f15 FC.+ finite 3

-- | Kind of a hack; we need to add a
-- | primitive to do this:
xfinite :: Finite 15 -> Finite 60
xfinite i | i FC.== finite 0  = finite 0
          | i FC.== finite 1  = finite 1
          | i FC.== finite 2  = finite 2
          | i FC.== finite 3  = finite 3
          | i FC.== finite 4  = finite 4
          | i FC.== finite 5  = finite 5
          | i FC.== finite 6  = finite 6
          | i FC.== finite 7  = finite 7
          | i FC.== finite 8  = finite 8
          | i FC.== finite 9  = finite 9
          | i FC.== finite 10 = finite 10
          | i FC.== finite 11 = finite 11
          | i FC.== finite 12 = finite 12
          | i FC.== finite 13 = finite 13
          | otherwise         = finite 14

body :: Finite 60 -> KeySchedule -> W 32
body i w = let
                wi1 , wi4 , temp :: W 32 
                wi1   = w `index` (i FC.- finite 1) -- == temp in Fig. 11.
                wi4   = w `index` (i FC.- finite 4)
                imod4 :: Finite 60
                imod4 = i `FC.mod` (finite 4)
                temp  = if (imod4 FC.== finite 0)
                          then
                             subword(rotword wi1) ^ (toW32 (rcon (ixdiv4 i)))      
                          -- else if (im8 FC.== finite 4)
                          --        then
                          --          subword wi1
                                 else
                                   wi1
            in
                 wi4 ^ temp

-- | effectively, this subtracts 1. Table5 is indexed by 1..10, and
-- | table5 below is indexed by 0..9.
ixdiv4 :: Finite 60 -> Finite 10
ixdiv4 i | idiv4 FC.== finite 0 = finite 0
         | idiv4 FC.== finite 1 = finite 0
         | idiv4 FC.== finite 2 = finite 1
         | idiv4 FC.== finite 3 = finite 2
         | idiv4 FC.== finite 4 = finite 3
         | idiv4 FC.== finite 5 = finite 4
         | idiv4 FC.== finite 6 = finite 5
         | idiv4 FC.== finite 7 = finite 6
         | idiv4 FC.== finite 8 = finite 7
         | idiv4 FC.== finite 9 = finite 8
         | otherwise            = finite 9 -- unreachable.
  where
    idiv4 :: Finite 60
    idiv4 = i `FC.div` (finite 4)

{- Working version -}
rcon :: Finite 10 -> Vec 4 (W 8)
rcon i = table5 `index` i -- (i FC.- (finite 1))

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

-- |
-- | Standard semantics for Key Expansion
-- |

keyexpand :: W 128 -> KeySchedule
keyexpand k = fst . rnd . rnd . rnd . rnd . rnd .
                    rnd . rnd . rnd . rnd . rnd .
                    rnd . rnd . rnd $ (initKeySched128 k , finite 4)

ks0 :: KeySchedule
ks0 = fromList
        [ lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 ]
