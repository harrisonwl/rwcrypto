{-# LANGUAGE DataKinds #-}
module Expansion (salsa20_k0k1, salsa20_k, salsa20_k0k1', salsa20_k', expandk0k1, expandk , there, back) where

import Prelude hiding ((++)) 
import ReWire 
import ReWire.Bits (lit)
import ReWire.Vectors (slice , (++))
import Basic (Quad, Hex, X16(..), X64(..))
import HashSalsa20 (hash_salsa20,hash)

b2w32 :: W 8 -> W 8 -> W 8 -> W 8 -> W 32
b2w32 x0 x1 x2 x3 = x0 ++ x1 ++ x2 ++ x3

there :: X64 (W 8) -> Hex (W 32)
there  (X64 x0  x1  x2  x3  x4  x5  x6  x7  x8  x9 x10 x11 x12 x13 x14 x15
            x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
            x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
            x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 )
              = X16 w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15
   where
     w0 , w1 , w2 , w3 , w4 , w5 , w6 , w7 , w8 , w9 , w10 , w11 , w12 , w13 , w14 , w15 :: W 32
     w0  = b2w32 x0 x1 x2 x3
     w1  = b2w32 x4 x5 x6 x7
     w2  = b2w32 x8 x9 x10 x11
     w3  = b2w32 x12 x13 x14 x15
     w4  = b2w32 x16 x17 x18 x19
     w5  = b2w32 x20 x21 x22 x23
     w6  = b2w32 x24 x25 x26 x27
     w7  = b2w32 x28 x29 x30 x31
     w8  = b2w32 x32 x33 x34 x35
     w9  = b2w32 x36 x37 x38 x39
     w10 = b2w32 x40 x41 x42 x43
     w11 = b2w32 x44 x45 x46 x47
     w12 = b2w32 x48 x49 x50 x51
     w13 = b2w32 x52 x53 x54 x55
     w14 = b2w32 x56 x57 x58 x59
     w15 = b2w32 x60 x61 x62 x63

-- |
-- | This is factor function tweeked so that it takes (W 64) as input instead of Integer. 
-- |
factor32 :: W 32 -> (W 8 , W 8 , W 8 , W 8)
factor32 w32 = (s0 , s1 , s2 , s3)
  where
    s0 , s1 , s2 , s3 :: W 8
    s0 = slice (Proxy :: Proxy 0)  w32
    s1 = slice (Proxy :: Proxy 8)  w32
    s2 = slice (Proxy :: Proxy 16) w32
    s3 = slice (Proxy :: Proxy 24) w32

back :: Hex (W 32) -> X64 (W 8) 
back (X16 w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15)
     = (X64 x0  x1  x2  x3  x4  x5  x6  x7  x8  x9 x10 x11 x12 x13 x14 x15
            x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
            x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
            x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 )
   where
     x0 , x1 , x2 , x3 , x4 , x5 , x6 , x7 , x8 , x9 , x10 , x11 , x12 , x13 , x14 , x15 ,
      x16 , x17 , x18 , x19 , x20 , x21 , x22 , x23 , x24 , x25 , x26 , x27 , x28 , x29 , x30 , x31 ,
      x32 , x33 , x34 , x35 , x36 , x37 , x38 , x39 , x40 , x41 , x42 , x43 , x44 , x45 , x46 , x47 ,
      x48 , x49 , x50 , x51 , x52 , x53 , x54 , x55 , x56 , x57 , x58 , x59 , x60 , x61 , x62 , x63 :: W 8
     ( x0 ,  x1 ,  x2 ,  x3) = factor32 w0
     ( x4 ,  x5 ,  x6 ,  x7) = factor32 w1
     ( x8 ,  x9 , x10 , x11) = factor32 w2
     (x12 , x13 , x14 , x15) = factor32 w3

     (x16 , x17 , x18 , x19) = factor32 w4
     (x20 , x21 , x22 , x23) = factor32 w5
     (x24 , x25 , x26 , x27) = factor32 w6
     (x28 , x29 , x30 , x31) = factor32 w7

     (x32 , x33 , x34 , x35) = factor32 w8
     (x36 , x37 , x38 , x39) = factor32 w9
     (x40 , x41 , x42 , x43) = factor32 w10
     (x44 , x45 , x46 , x47) = factor32 w11

     (x48 , x49 , x50 , x51) = factor32 w12
     (x52 , x53 , x54 , x55) = factor32 w13
     (x56 , x57 , x58 , x59) = factor32 w14
     (x60 , x61 , x62 , x63) = factor32 w15


-----------------------------
-- Salsa20 Expansion Functions (Sect 9, p8)
-----------------------------

salsa20_k0k1 :: (Hex (W 8), Hex (W 8)) -> Hex (W 8) -> X64 (W 8)
salsa20_k0k1 (k0,k1) n = hash_salsa20 (expandk0k1 (k0 , k1 , n))

salsa20_k :: Hex (W 8) -> Hex (W 8) -> X64 (W 8)
salsa20_k k n = hash_salsa20 (expandk k n)

salsa20_k0k1' :: (Hex (W 8), Hex (W 8)) -> Hex (W 8) -> Hex (W 32)
salsa20_k0k1' (k0,k1) n = hash (there (expandk0k1 (k0 , k1 , n)))

salsa20_k' :: Hex (W 8) -> Hex (W 8) -> Hex (W 32)
salsa20_k' k n = hash (there (expandk k n))

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

