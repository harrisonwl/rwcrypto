{-# LANGUAGE DataKinds #-}
module Aes.KeyExp.Reference256 where

import Prelude as P hiding ((-) , (*) , (<) , (^) , (/) , head , tail , round)
import ReWire hiding (put , get , signal , lift)
import ReWire.Bits as RB hiding ((<) , (*))
import ReWire.Vectors hiding (update , (!=))
import ReWire.Finite
import ReWire.FiniteComp as FC
import Aes.ExtensionalSemantics

import Aes.Basic((!=) , (@@@)  , toW32 )
import Aes.SubBytes(subword)
import Aes.RotWord(rotword)

---
--- Intended to be the standard semantics for AES-256 key expansion.
---

-- |
-- | AES-256 instance
-- | N.b., in Fig. 11, w[8] depends on w[7], then w[9] depends on w[8], etc., and,
-- | consequently, this can't be written as a map, Finite 60 -> W 32. Or, in other words,
-- | it is truly iterative.
-- |

-- | This is for defining the KeyExpansion routine from Fig 11, page 20, of nist.fips.197.
-- 
--           Nk   Nb   Nr   KeyExpansion(byte key[4*nk] , word w[nb*(nr+1)])
-- -------------------------------------------------------------------------
-- AES-128 |  4 |  4 | 10 | KeyExpansion(byte key[16] , word w[44])
-- -------------------------------------------------------------------------
-- AES-192 |  6 |  4 | 12 | KeyExpansion(byte key[24] , word w[52])
-- -------------------------------------------------------------------------
-- AES-256 |  8 |  4 | 14 | KeyExpansion(byte key[32] , word w[60])

-- |
-- | N.b., we represent the type of key as (8 x W 32) rather than (32 x W 8)
-- | It's way more sensible.
-- |

type KeySchedule = Vec 60 (W 32)
type Key         = Vec 8 (W 32) 
type RF          = (KeySchedule, W 6)

-- | To sort that out, here are the types for the
-- | various key sizes.
-- -------------------------------------------------------
-- AES-128 | word key[4] , word w[44] | 44 - 4 = 40
-- -------------------------------------------------------
-- AES-192 | word key[6] , word w[52] | 52 - 6 = 46
-- -------------------------------------------------------
-- AES-256 | word key[8] , word w[60] | 60 - 8 = 52
-- -------------------------------------------------------


-- |
-- | Standard semantics for Key Expansion
-- |
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

ks0 :: KeySchedule
ks0 = fromList
        [ lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 ]
-- |
-- | experimenting with Key = Vec 8 (W 32) instead of Vec 32 (W 8)

initKS :: Vec 8 (W 32) -> KeySchedule -> KeySchedule
initKS k = mv2ks (lit 0) k .
           mv2ks (lit 1) k .
           mv2ks (lit 2) k .
           mv2ks (lit 3) k .
           mv2ks (lit 4) k . 
           mv2ks (lit 5) k . 
           mv2ks (lit 6) k . 
           mv2ks (lit 7) k 
   where

     mv2ks :: W 6 -> Vec 8 (W 32) -> Vec 60 (W 32) -> Vec 60 (W 32)
     mv2ks i k w = w != toFinite i $ k `index` (toFinite i)

--

assign :: W 6 -> W 32 -> KeySchedule -> KeySchedule
assign c w32 ks = ks != (toFinite c) $ w32

round :: ST RF ()
round = do
           rf <- get
           put (rnd rf)

-- |
-- | Purely functional version of round.
-- | N.b., it "unrolls" the loop 4 times each.
-- |
rnd :: (KeySchedule, W 6) -> (KeySchedule, W 6)
rnd = expand . expand . expand . expand
  where
    expand :: (KeySchedule, W 6) -> (KeySchedule, W 6)
    expand (ks , c) = (assign c (body c ks) ks , c RB.+ lit 1)

body :: W 6 -> KeySchedule -> W 32
body i w = let
               wi1 , wi8 , temp :: W 32 
               imod8 :: W 6
               wi1   = w @@@ (i RB.- lit 1) -- == temp in Fig. 11.
               wi8   = w @@@ (i RB.- lit 8)
               imod8 = i % (lit 8)
               temp  = if (imod8 RB.== lit 0)
                        then
                           subword(rotword wi1) ^ (toW32 (rcon (i / (lit 8))))
                        else if (imod8 RB.== lit 4)
                               then
                                 subword wi1
                               else
                                 wi1
           in
               wi8 ^ temp

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


{-
initKS :: Key -> KeySchedule -> KeySchedule
initKS k = expand (lit 0) k .
           expand (lit 1) k .
           expand (lit 2) k .
           expand (lit 3) k .
           expand (lit 4) k . 
           expand (lit 5) k . 
           expand (lit 6) k . 
           expand (lit 7) k 

   where

     expand :: W 6 -> Vec 32 (W 8) -> Vec 60 (W 32) -> Vec 60 (W 32)
     expand i k w = w != toFinite i $ merge k (toFinite i)

     merge :: Vec 32 (W 8) -> Finite 32 -> W 32
     merge k i = (k `index` i4) ReWire.Vectors.++ (k `index` i41) ReWire.Vectors.++ (k `index` i42) ReWire.Vectors.++ (k `index` i43)
       where
         i4 , i41 , i42 , i43 :: Finite 32
         i4  = finite 4 * i
         i41 = finite 4 * i FC.+ finite 1
         i42 = finite 4 * i FC.+ finite 2
         i43 = finite 4 * i FC.+ finite 3
-}

-- boost :: ST RF a -> ST (Key , KeySchedule, W 6) a
-- boost (ST f) = ST $ \ (_ , ks , c) ->
--                           let
--                             (a , (ks' , c')) = f (ks , c)
--                           in
--                             (a , (k0 , ks' , c'))

-- expand >> expand >> expand >> expand   

  -- where
    
  --   expand :: ST RF ()
  --   expand = do
  --              (ks , c ) <- get
  --              let w32 = body c ks
  --              put (assign c w32 ks , c RB.+ lit 1)
