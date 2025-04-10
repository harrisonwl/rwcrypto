{-# LANGUAGE DataKinds #-}
module Expansion (salsa20_k0k1, salsa20_k, alltests) where

import ReWire 
import ReWire.Bits (lit)

import Idioms (Quad, Hex, X16(..), X64(..))
import HashSalsa20 (hash_salsa20)

import ReWire.Vectors as RV
import qualified ReWire.Interactive as RI

help :: (Functor f, RI.ShowDec a) => f a -> f String
help x64 = fmap RI.dshow x64

mkW :: (Functor f, KnownNat n) => f Integer -> f (W n)
mkW = fmap lit

-----------------------------
-- Salsa20 Expansion Functions (Sect 9, p8)
-----------------------------

-- σ0 = (101,120,112,97),
-- σ1 = (110,100,32,51),
-- σ2 = (50,45,98,121), and
-- σ3 = (116,101,32,107)

salsa20_k0k1 :: (Hex (W 8), Hex (W 8)) -> Hex (W 8) -> X64 (W 8)
salsa20_k0k1 (k0,k1) n = hash_salsa20 (expandk0k1 (k0 , k1 , n))

salsa20_k :: Hex (W 8) -> Hex (W 8) -> X64 (W 8)
salsa20_k k n = hash_salsa20 (expandk k n)

expandk0k1 :: (Hex (W 8), Hex (W 8), Hex (W 8)) -> X64 (W 8)
expandk0k1 
       ( (X16 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16) -- k0
       , (X16 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16) -- k1
       , (X16 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16) -- n
       ) = let
              sigma0 , sigma1 , sigma2 , sigma3 :: Quad (W 8)
              sigma0@(x1,x2,x3,x4) = (lit 101, lit 120, lit 112,  lit 97)
              sigma1@(w1,w2,w3,w4) = (lit 110, lit 100,  lit 32,  lit 51)
              sigma2@(v1,v2,v3,v4) = ( lit 50,  lit 45,  lit 98, lit 121)
              sigma3@(t1,t2,t3,t4) = (lit 116, lit 101,  lit 32, lit 107)
           in
             X64
                 x1 x2 x3 x4
                 --
                 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16
                 --
                 w1 w2 w3 w4
                 --
                 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16
                 --
                 v1 v2 v3 v4
                 --
                 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
                 --
                 t1 t2 t3 t4
                 --

expandk :: Hex (W 8) -> Hex (W 8) -> X64 (W 8)
expandk k@(X16 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16)
        n@(X16 n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16)
       = let
             tau0 , tau1 , tau2 , tau3 :: Quad (W 8)
             tau0@(x1 , x2 , x3 , x4) = (lit 101,lit 120,lit 112,lit 97)
             tau1@(y1 , y2 , y3 , y4) = (lit 110,lit 100,lit 32,lit 49)
             tau2@(u1 , u2 , u3 , u4) = (lit 54,lit 45,lit 98,lit 121)
             tau3@(v1 , v2 , v3 , v4) = (lit 116,lit 101,lit 32,lit 107)
         in
           X64
               --- tau0
               x1 x2 x3 x4 
               --- k
               k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16
               --- tau1
               y1 y2 y3 y4
               --- n
               n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16
               --- tau2
               u1 u2 u3 u4
               --- k
               k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16
               --- tau3
               v1 v2 v3 v4


alltests :: [Bool]
alltests = [test1 , test2 , test3 , test4]

k0 , n , k1 :: Hex (W 8)
k0 = mkW $ X16 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
k1 = mkW $ X16 201 202 203 204 205 206 207 208
               209 210 211 212 213 214 215 216
n  = mkW $ X16 101 102 103 104 105 106 107 108
               109 110 111 112 113 114 115 116

-- | Checks that the first example is prepared correctly.
test1 :: Bool
test1 = expandk0k1 (k0 , k1 , n) == x1
  where
    x1 :: X64 (W 8)
    x1 = mkW $ X64
                  101 120 112  97  1  2  3  4  5  6  7  8  9  10  11  12 
                  13  14  15  16 110 100  32  51 101 102 103 104 105 106 107 108 
                  109 110 111 112 113 114 115 116  50  45  98 121 201 202 203 204 
                  205 206 207 208 209 210 211 212 213 214 215 216 116 101  32 107

test2 :: Bool
test2 = salsa20_k0k1 (k0 , k1) n == o1
  where
    o1 :: X64 (W 8)
    o1 = fmap lit $ X64
                        69  37  68  39  41  15 107 193 255 139 122  6 170 233 217  98 
                        89 144 182 106  21  51 200  65 239  49 222  34 215 114  40 126 
                        104 197  7 225 197 153  31  2 102  78  76 176  84 245 246 184 
                        177 160 133 130  6  72 149 119 192 195 132 236 234 103 246  74

-- | Checks that the second example is prepared correctly.               
test3 :: Bool
test3 = expandk k0 n == x3
  where
    x3 :: X64 (W 8)
    x3 = mkW $ X64
      101 120 112  97  1  2  3  4  5  6  7  8  9  10  11  12 
       13  14  15  16 110 100  32  49 101 102 103 104 105 106 107 108 
      109 110 111 112 113 114 115 116  54  45  98 121  1  2  3  4 
      5  6  7  8  9  10  11  12  13  14  15  16 116 101  32 107

test4 = salsa20_k k0 n == o2
  where
    o2 :: X64 (W 8)
    o2 = mkW $ X64
      39 173  46 248  30 200  82  17  48  67 254 239  37  18  13 247 
      241 200  61 144  10  55  50 185  6  47 246 253 143  86 187 225 
      134  85 110 246 161 163  43 235 231  94 171  51 145 214 112  29 
      14 232  5  16 151 140 183 141 171  9 122 181 104 182 177 193
