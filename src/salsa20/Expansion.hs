{-# LANGUAGE DataKinds #-}
module Expansion (salsa20_k0k1, salsa20_k, expandk0k1, expandk) where

import ReWire 
import ReWire.Bits (lit)
import Basic (Quad, Hex, X16(..), X64(..))
import HashSalsa20 (hash_salsa20)

-----------------------------
-- Salsa20 Expansion Functions (Sect 9, p8)
-----------------------------

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

