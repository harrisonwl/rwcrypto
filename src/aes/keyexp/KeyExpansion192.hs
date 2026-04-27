{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Aes.KeyExp.KeyExpansion192 {- ( keyexpand
                                  , roundkey
                                  , initKeySched192
                                  , ks0
                                  , rnd
                                  , RF ) -}
   where

import Prelude as P hiding ((-) , (*) , (<) , (^) , (/) , head , tail , round)
import ReWire -- hiding (put , get , signal , lift)
import ReWire.Bits as RB hiding ((<) , (*) , (==))
import ReWire.Vectors hiding ((!=))
import ReWire.Finite
import ReWire.FiniteComp as FC

import Aes.Basic(Key , KeySchedule , RoundKey , roundkey , (!=) , toW32 , splitkey192 , toByte4 , transpose)
import Aes.Operations.SubBytes(subword)
import Aes.Operations.RotWord(rotword)

-- import ReWire.Interactive (dshow , hex , xshow)

--          Key Length (Nk words) | Block Size (Nb words) | Number of Rounds (Nr)
-- -------------------------------------------------------------------------------
-- AES-128 |          4           |           4           |        10
-- -------------------------------------------------------------------------------
-- AES-192 |          6           |           4           |        12
-- -------------------------------------------------------------------------------
-- AES-256 |          8           |           4           |        14

-- | For AES-192, Nb * (Nr + 1) = 4 * (12 + 1) = 52
-- |
-- | Note that we haven't changed the size of KeySchedule.
-- | 

initKeySched192 :: W 192 -> KeySchedule
initKeySched192 k = splitkey192 k ReWire.Vectors.++ ks54
  where
    -- | This is for the initialization of the keyschedule. 
    ks54 :: Vec 54 (W 32)
    ks54 = fromList
             [ lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
             , lit 0 , lit 0 , lit 0 , lit 0 ]

type RF          = (KeySchedule, Finite 54)


-- | effectively, this subtracts 1. Table5 is indexed by 1..10, and
-- | table5 below is indexed by 0..9.
-- | Kind of a hack; we need to add a
-- | primitive to do this:
ixdiv6 :: Finite 60 -> Finite 10
ixdiv6 i | idiv6 FC.== finite 0 = finite 0
         | idiv6 FC.== finite 1 = finite 0
         | idiv6 FC.== finite 2 = finite 1
         | idiv6 FC.== finite 3 = finite 2
         | idiv6 FC.== finite 4 = finite 3
         | idiv6 FC.== finite 5 = finite 4
         | idiv6 FC.== finite 6 = finite 5
         | idiv6 FC.== finite 7 = finite 6
         | idiv6 FC.== finite 8 = finite 7
         | idiv6 FC.== finite 9 = finite 8
         | otherwise            = finite 9 -- unreachable.
  where
    idiv6 :: Finite 60
    idiv6 = i `FC.div` (finite 6)

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
-- | Standard semantics for Key Expansion
-- |

-- | N.b., this fills in the whole KeySchedule, although only
-- | the first 52 elements are used. Should probably trim that.
keyexpand :: W 192 -> KeySchedule
keyexpand k = fst . rnd . rnd . rnd . rnd . rnd .
                    rnd . rnd . rnd . rnd . rnd .
                    rnd . rnd . rnd $ (initKeySched192 k , finite 6)

body :: Finite 60 -> KeySchedule -> W 32
body i w = let
                wi1 , wi6 , temp :: W 32 
                wi1   = w `index` (i FC.- finite 1) -- == temp in Fig. 11.
                wi6   = w `index` (i FC.- finite 6)
                imod6 :: Finite 60
                imod6 = i `FC.mod` (finite 6)
                temp  = if (imod6 FC.== finite 0)
                          then
                             subword(rotword wi1) ^ (toW32 (rcon (ixdiv6 i)))      
                          -- else if (im8 FC.== finite 4)
                          --        then
                          --          subword wi1
                                 else
                                   wi1
            in
                 wi6 ^ temp

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


ks0 :: KeySchedule
ks0 = fromList
        [ lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 ]

