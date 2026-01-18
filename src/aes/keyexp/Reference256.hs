{-# LANGUAGE DataKinds #-}
module Aes.KeyExp.Reference256 where

import Prelude as P hiding ((-) , (*) , (==) , (<) , (^) , (/) , head , tail , round)
import ReWire hiding (put , get , signal , lift)
import ReWire.Bits as RB hiding ((<) , (*))
import ReWire.Vectors hiding (update , (!=))
import ReWire.Finite
import ReWire.FiniteComp as FC
import Aes.ExtensionalSemantics

import Aes.Basic({- update , -} (!=) , (@@@)  , toW32 {-, toByte4-})
import Aes.SubBytes(subword)
import Aes.RotWord(rotword)

import ReWire.Interactive({-Pretty ,-} pretty {- , pp-})

type KeySchedule = Vec 60 (W 32)
type Key         = Vec 32 (W 8) 

---
--- Intended to be the standard semantics for AES-256 key expansion.
---

------
-- Standard Semantics
------

go :: Integer -> IO ()
go i = pretty $ fst $ runST (keyexpand i k0) (undefined, undefined)

-- |
-- | Standard semantics for Key Expansion
-- |
keyexpand :: Integer -> Key -> ST (KeySchedule, W 6) (W 32)
keyexpand i k = do
                  put (initKS k ks0 , lit 8)
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
                  round
                  round
                  rdKS (finite i)

rdKS :: Finite n -> ST (Vec n w, s) w
rdKS i = do
            (ks , _) <- get
            return (ks `index` i)

-- From both nist.fips.197-upd1 (Appendix A3) and AES_Core256
keyex :: W 256
keyex = lit 0x603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4

-- k0 is keyex ^^^ split into bytes
k0 :: Key
k0 = fromList
        [ lit 0x60 , lit 0x3d , lit 0xeb , lit 0x10 , lit 0x15 , lit 0xca , lit 0x71 , lit 0xbe
        , lit 0x2b , lit 0x73 , lit 0xae , lit 0xf0 , lit 0x85 , lit 0x7d , lit 0x77 , lit 0x81
        , lit 0x1f , lit 0x35 , lit 0x2c , lit 0x07 , lit 0x3b , lit 0x61 , lit 0x08 , lit 0xd7
        , lit 0x2d , lit 0x98 , lit 0x10 , lit 0xa3 , lit 0x09 , lit 0x14 , lit 0xdf , lit 0xf4 ]

ks0 :: KeySchedule
ks0 = fromList
        [ lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0
        , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 , lit 0 ]

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
     merge key i = (key `index` i4) ReWire.Vectors.++ (key `index` i41) ReWire.Vectors.++ (key `index` i42) ReWire.Vectors.++ (key `index` i43)
       where
         i4 , i41 , i42 , i43 :: Finite 32
         i4  = finite 4 * i
         i41 = finite 4 * i FC.+ finite 1
         i42 = finite 4 * i FC.+ finite 2
         i43 = finite 4 * i FC.+ finite 3

assign :: W 6 -> W 32 -> KeySchedule -> KeySchedule
assign c w32 ks = ks != (toFinite c) $ w32

round :: ST (KeySchedule, W 6) ()
round = expand >> expand >> expand >> expand

  where
    
    expand :: ST (KeySchedule, W 6) ()
    expand = do
               (ks , c ) <- get
               let w32 = body c ks
               put (assign c w32 ks , c RB.+ lit 1)

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

w0 :: KeySchedule
w0 = initKS k0 ks0

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
